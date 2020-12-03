
import AST._ 
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.immutable.Vector
import scala.collection.mutable.ListBuffer

import scala.math.BigInt
import java.lang.Thread.State

//import cats.data.Writer
//import cats.syntax.writer._

import cats.instances.vector._
import cats.syntax.applicative._ 
import cats.syntax.either._ 

case class PropertyStore(name: String) {
    var table = new HashMap[String, Property]()

    override def toString: String = s"${name} :: ${table}"

    def setRange(r: Option[Range]): PropertyStore = {
        for {
            range <- r
        } yield table.put("range", range)
        this 
    }

    def setSubrange(sr: Option[Range]): PropertyStore = {
        for {
            subrange <- sr
        } yield table.put("subrange", subrange)
        this 
    }

    def setType(t: Type): PropertyStore = {
        table.put("type", t)
        this 
    }

    def setArity(a: Int): PropertyStore = { 
        table.put("arity", Arity(a))
        this
    }

    def hasType(t: Type): Boolean = table.get("type") match {
        case Some(actualType) => t == actualType
        case None => false
    }

    def getAttribute(attr: String): Option[Property] = table.get(attr)
}

case class SymbolTable() {
    var table = new HashMap[String, PropertyStore]()

    def nameDefined(name: String): Boolean = table.get(name) match {
        case Some(propertyStore) => true
        case None => false
    }

    def defineName(name: String): Unit = table.put(name, PropertyStore(name))
    def setPropertiesForName(name: String, p: PropertyStore): Unit = table.put(name, p)

    def getPropertyForName(name: String, propertyName: String): Option[Property] =
        for {
            properties  <- table.get(name)
            t <- properties.getAttribute(propertyName)
        } yield t

    def getTypeForName(name: String): Type = getPropertyForName(name, "type") match {
            case Some(t) => t match {
                case t: Type => t 
                case _ => ErrorType
            }
            case None => ErrorType
        }

    override def toString: String = table.toString
}

object TypeChecker {
    type Errors = ListBuffer[String]
    type _Error = String
    type _Result = Either[_Error, Type]
    type Result = (Type, Errors)

    type Writer[A] = (Errors, A)
    def unit[A](a: A): Writer[A] = (ListBuffer(), a)
    def error(msg: String): Writer[Unit] = (ListBuffer(msg), ())
    def flatMap[A, B](a: Writer[A], b: A => Writer[B]): Writer[B] = {
        val (errs, value) = a
        val (newerrs, newvalue) = b(value)
        (errs ++ newerrs, newvalue)
    }

    var table: SymbolTable = new SymbolTable()
    var errors: Errors = ListBuffer()
    var funcStack: Stack[String] = Stack()

    def check(prog: Program): Writer[Type] = {
        prog.funcs.foreach(checkFuncSignature(_))
        prog.funcs.foreach(checkFuncBody(_))
        (errors, ProgramType)
    }

    def checkFuncSignature(func: FuncDecl): Writer[Type] = 
        func match {
            case FuncDecl(typ, range, name, args, body) => {
                val p = PropertyStore(name)
                            .setRange(range)
                            .setType(typ)
                            .setArity(args.length)

                if (table.nameDefined(name))
                    errors += s"Func ${name} already defined"

                table.setPropertiesForName(name, p)

                args.foreach(checkArg)
                checkAllDeclsHaveRhs(body)

                (errors, typ)
            }
        }

    def checkFuncBody(func: FuncDecl): Writer[Type] =
        func match {
            case FuncDecl(typ, range, name, args, body) => {
                funcStack.push(name)
                body.foreach(checkStmt)
                funcStack.pop()
                (errors, typ)
            }
        }

    def checkStmt(stmt: Statement): Writer[Type] =
        stmt match {
            case Return(expr) => {
                val (preverrors, typ) = checkExpr(expr)
                val currentFunc = funcStack.top
                val expectedReturnType = table.getPropertyForName(currentFunc, "type")

                if (!expectedReturnType.contains(typ))
                    errors += s"Return type of ${expr} doesnt match expected ${expectedReturnType}"

                (errors, typ)
            }

            case For(iter1, iter2, acc, body) => {
                checkIterator(iter1)
                iter2 match {
                    case Some(iter) => checkIterator(iter)
                    case None => None
                }
                acc match {
                    case Decl(_, _, _) => checkStmt(acc)
                    case _ => errors += s"Accumulator for range ${stmt} malformed"
                }
                checkLoopBody(body)
            }

            case If(cond, thn, els) => {
                val (currentErrors, condType) = checkExpr(cond)
                if (condType != IntType)
                    errors += s"Expected int type in if condition ${cond}"
                thn.foreach(checkStmt)
                for {
                    stmts <- els
                } yield stmts.foreach(checkStmt)
                (errors, condType)
            }
            case Decl(t, name, expr) => {
                if (table.nameDefined(name))
                    errors += s"Cannot redefine ${name} at ${Decl(t, name, expr)}"

                val (currentErrors, rhsType) = expr match {
                    case None => (errors, t)
                    case Some(rhs) => checkExpr(rhs)
                }

                if (rhsType != t)
                    errors += s"expected type ${t}, found ${expr}"

                val p = PropertyStore(name).setType(t)
                table.setPropertiesForName(name, p)

                (errors, t)
            }
        }

    def checkLoopBody(body: List[ForBody]): _Result = body match {
        case Nil => Left(s"Function bodies cannot be empty")
        case x :: Nil => x match {
            case expr: Expr => checkExpr(expr)
            case _ => Left(s"Last computation in for loop must be an expr and not a statement")
        }
        case x :: xs => {
            x match {
                case s : Statement => checkStmt(s)
                case _ => Left(s"Intermediate computations in for loops must be statements")
            }
            checkLoopBody(xs)
        }
    }

    def checkIterator(iter: Iterator): _Result = 
        iter match {
            case Iterator(idx, itere) => {         
                val expectedType = for {
                    t <- checkExpr(itere)
                    _ <- if (table.nameDefined(idx)) Left(s"Iterator ${idx} shadows another variable and is not allowed")
                         else Right(ErrorType)
                    expectedIdxType <- if (t == IntIterType) Right(IntType)
                                       else if (t == StrIterType) Right(StringType)
                                       else Left(s"Can only interate over iterator types in '${idx} : ${itere}'")
                } yield expectedIdxType

                expectedType match {
                    case Left(err) => Left(err)
                    case Right(t) => {
                        val p = PropertyStore(idx).setType(t)
                        table.setPropertiesForName(idx, p)
                        Right(t)
                    }
                }
            }
        }

    def checkArg(arg: Arg): Writer[Type] = {
        val typ = arg.t
        val subrange = arg.subrange
        val range = arg.range
        val name = arg.name

        subrange match {
            case Some(range) =>
                if (typ == IntType || typ == StringType)
                    errors += s"${name} is a ${typ} which cannot have a subrange"
            case None => {}
        }

        val p = PropertyStore(name)
                    .setType(typ)
                    .setRange(range)
                    .setSubrange(subrange)

        if (table.nameDefined(name))
            errors += s"Arg name${name} already previously defined"

        table.setPropertiesForName(name, p)

        (errors, typ)
    }

    def checkExpr(expr: Expr): Either[_Error, Type] = {
        expr match {
            case Add(l, r) => checkNumericBinOp(expr, l, r)
            case Minus(l,r) => checkNumericBinOp(expr, l, r)
            case Mult(l,r) => checkNumericBinOp(expr, l, r)
            case Div(l,r) => checkNumericBinOp(expr, l, r)
            case Eq(l,r) => checkNumericBinOp(expr, l, r)
            case Neq(l,r) => checkNumericBinOp(expr, l, r)
            case Gte(l,r) => checkNumericBinOp(expr, l, r)
            case Lte(l,r) => checkNumericBinOp(expr, l, r)
            case Gt(l,r) => checkNumericBinOp(expr, l, r)
            case Lt(l, r) => checkNumericBinOp(expr, l, r)
            case Or(l,r) => checkNumericBinOp(expr, l, r)
            case And(l, r) => checkNumericBinOp(expr, l, r)
            case Not(l) => checkNumericUnOp(expr, l)
            case Ntos(l) => checkNumericUnOp(expr, l)

            case Ston(l) => checkStringUnop(expr, l)
            case StrSplit(l,r) => checkStringBinOp(expr, l, r)
            case StrEquals(l, r) => checkStringBinOp(expr, l, r)
            case StrConcat(l, r) => checkStringBinOp(expr, l, r)
            case StrAppend(l, r) => checkStringBinOp(expr, l, r)
            case StrLength(l) => checkStringUnop(expr, l)

            case IterConcat(l,r) => checkIterBinOp(expr, l, r)
            case IterFirst(l) => checkIterUnOp(expr, l)
            case IterLength(l) => checkIterUnOp(expr, l)

            case FunCall(name, params) => {
                if (!table.nameDefined(name))
                    Left(s"Function ${name} not defined before use")
                else {
                    val expectedArity: Option[Property] = table.getPropertyForName(name, "arity")
                    val expectedType: Type = table.getTypeForName(name)

                    expectedArity match {
                        case Some(prop) => prop match {
                            case Arity(num) => if (num != params.length) Left(s"Arity mismatch for function call ${expr}")
                                               else Right(expectedType)
                            case _ => Left(s"Expected arity at ${expectedArity}")
                        }
                        case None => Left(s"Couldn't figure out arity for function call ${expr}")
                    }
                }
            }

            case Var(name) => if (table.nameDefined(name)) Right(table.getTypeForName(name))
                              else Left(s"Var ${name} not defined before use")
            case IntLit(num, width, signed, range) => 
                for {
                    rangeCheck <- rangeOk(range)
                    intCheck <- intOk(num, width, signed, range)
                    retType <- if (intCheck) Right(IntType)
                               else Left(s"Int lit ${IntLit(num, width, signed, range)} invalid")
                } yield retType
            case StrLit(value, range) => rangeOk(range).flatMap(_ => Right(StringType))
            case IntIter(value, range) => rangeOk(range).flatMap(_ => Right(IntIterType))
            case StrIter(value, range) => rangeOk(range).flatMap(_ => Right(StrIterType))
        }
    }

    def checkNumericBinOp(e: Expr, l: Expr, r: Expr): _Result = 
        for {
            ltype <- checkExpr(l) 
            _ <- if (ltype != IntType) Left(s"Expected expression of type of int at ${l}")
                 else Right(IntType)
            rtype <- checkExpr(r)
            _ <- if (rtype != IntType) Left(s"Expected expression of type of int at ${r}")
                 else Right(IntType)
        } yield IntType

    def checkStringBinOp(e: Expr, l: Expr, r: Expr): _Result = {
        val returnType = e match {
            case StrSplit(_, _) => StrIterType
            case StrEquals(_, _) => IntType
            case _ => StringType
        }
        for {
            ltype <- checkExpr(l)
            _ <- if (ltype != StringType) Left(s"Expected expression of type of string at ${l}")
                 else Right(StringType)
            rtype <- checkExpr(r)
            _ <- if (rtype != StringType) Left(s"Expected expression of type of string at ${r}")
                 else Right(StringType)
        } yield returnType
    }

    def checkIterBinOp(e: Expr, l: Expr, r: Expr): _Result = 
        for {
            ltype <- checkExpr(l)
            _ <- if (ltype != StrIterType && ltype != IntIterType) Left(s"Expected expression of type of iterator at ${l}")
                 else Right(IntIterType) // intermediate, doesn't matter
            rtype <- checkExpr(r)
            _ <- if (rtype != StrIterType && rtype != IntIterType) Left(s"Expected expression of type of iterator at ${r}")
                 else Right(IntIterType) // intermediate, doesn't matter
            returnType <- if (ltype != rtype) Left(s"Expected expressions ${l} and ${r} to have same iter type")
                          else Right(ltype)
        } yield returnType 
 
    def checkIterUnOp(e: Expr, l: Expr): _Result = 
        for {
            ltype <- checkExpr(l)
            check <- if (ltype != StrIterType && ltype != IntIterType) Left(s"Expected expression of type of iter at ${l}")
                     else Right(ltype)
            returnType <- inferIterUnOpType(e, ltype)
        } yield returnType

    def inferIterUnOpType(parentExp: Expr, leftType: Type): _Result = parentExp match {
        case IterLength(_) => Right(IntType)
        case IterFirst(_) => leftType match {
            case StrIterType => Right(StringType)
            case IntIterType => Right(IntType)
            case _ => Left(s"iter_first requires an iterator as its argument")
        }
    }

    def checkStringUnop(e: Expr, l: Expr): _Result =
        for {
            ltype <- checkExpr(l)
            check <- if (ltype != StringType) Left(s"Expected expression of type of string at ${l}")
                     else Right(ltype)
        } yield check

    def checkNumericUnOp(e: Expr, l: Expr): _Result = {
        val returnType = e match {
            case Ntos(_) => (errors, StringType)
            case _ => (errors, IntType) 
        }
        for {
            ltype <- checkExpr(l)
            check <- if (ltype != IntType) Left(s"Expected expression of type of int at ${l}")
                     else Right(ltype)
        } yield check
    }

    def intOk(num: Int, width: Int, signed: Boolean, range: Option[Range]): Either[_Error, Boolean] = {
        var lowerBound = if (signed) BigInt(-(2 ^ (width - 1))) else BigInt(0)
        var upperBound = if (signed) BigInt(2 ^ (width - 1) - 1) else BigInt(2 ^ width - 1)

        var upperRangeBound = upperBound
        var lowerRangeBound = lowerBound 

        range match {
            case Some(Range(lo, hi)) => {
                lowerRangeBound = BigInt(lo)
                upperRangeBound = BigInt(hi)
            }
            case None => {}
        }

        lowerBound = lowerBound.max(lowerRangeBound)
        upperBound = upperBound.min(upperRangeBound)

        val normalizedNum = BigInt(num)

        Right(lowerBound <= normalizedNum && normalizedNum <= upperBound)
    }

    def rangeOk(range: Option[Range]): Either[_Error, Boolean] = range match {
        case Some(Range(lo, hi)) => 
            if (lo > hi) Left(s"Lower bound on range must be less than or equal to upper bound: ${Range(lo, hi)}")
            else Right(true)
        case None => Right(true)
    }

    def checkAllDeclsHaveRhs(funcBody: List[Statement]): Unit = funcBody match {
        case Nil => ()
        case x :: xs => {
            x match {
                case Decl(t, name, expr) => expr match {
                    case None => errors += s"Decl ${x} must have a RHS"
                    case Some(_) => ()
                }
                case _ => ()
            }
            checkAllDeclsHaveRhs(xs)
        }
    }

}

object Test {
    import TypeChecker._

    def main(args: Array[String]): Unit = {
        val res = check(dimacs_scanner)
        println(res)
    }
}