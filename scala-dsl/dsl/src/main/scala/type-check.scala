
import AST._ 
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.immutable.Vector

import scala.math.BigInt

//import cats.data.Writer
//import cats.syntax.writer._
//import cats.instances.vector._
//import cats.syntax.applicative._ // for pure

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
    type Errors = Vector[String]
    type Result = (Type, Errors)

    type Writer[A] = (Errors, A)
    def unit[A](a: A): Writer[A] = (Vector(), a)
    def error(msg: String): Writer[Unit] = (Vector(msg), ())
    def flatMap[A, B](a: Writer[A], b: A => Writer[B]): Writer[B] = {
        val (errs, value) = a
        val (newerrs, newvalue) = b(value)
        (errs ++ newerrs, newvalue)
    }

    var table: SymbolTable = new SymbolTable()
    var errors: Errors = Vector()
    var funcStack: Stack[String] = Stack()

    def checkFunc(func: FuncDecl): Writer[Type] = 
        func match {
            case FuncDecl(typ, range, name, args, body) => {
                val p = PropertyStore(name)
                            .setRange(range)
                            .setType(typ)
                            .setArity(args.length)

                if (table.nameDefined(name))
                    errors ++ s"Func ${name} already defined"

                table.setPropertiesForName(name, p)
                funcStack.push(name)

                args.foreach(checkArg)
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
                    errors ++ s"Return type of ${expr} doesnt match expected ${expectedReturnType}"

                (errors, typ)
            }

            case For(iter1, iter2, acc, body) => (errors, StringType)
            case If(cond, thn, els) => (errors, StringType)
            case Decl(t, name, expr) => (errors, StringType)
        }


    def checkIterator(iter: Iterator): Writer[Type] = 
        iter match {
            case Iterator(idx, itere) => {                
                val (errors, t) = checkExpr(itere)
                var expectedIdxType: Type = IntType

                if (table.nameDefined(idx))
                   errors ++ s"Iterator ${idx} shadows another variable and is not allowed"

                t match {
                    case IntIterType => expectedIdxType = IntType
                    case StrIterType => expectedIdxType = StringType
                    case _ => errors ++ s"Can only iterate over iterator types in '${idx} : ${itere}'"
                }

                val p = PropertyStore(idx).setType(expectedIdxType)
                table.setPropertiesForName(idx, p)

                (errors, t)
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
                    errors ++ s"${name} is a ${typ} which cannot have a subrange"
            case None => {}
        }

        val p = PropertyStore(name)
                    .setType(typ)
                    .setRange(range)
                    .setSubrange(subrange)

        if (table.nameDefined(name))
            errors ++ s"Arg name${name} already previously defined"

        table.setPropertiesForName(name, p)

        (errors, typ)
    }

    def checkExpr(expr: Expr): Writer[Type] = {
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
                    // checkFunc(name) TODO:
                    (errors, ErrorType)
                else {
                    val expectedArity: Option[Int] = table.getPropertyForName(name, "arity")
                    val expectedType: Type = table.getTypeForName(name)
                    expectedArity match {
                        case Some(arity) => if (arity != params.length)
                                                errors ++ s"Arity mismatch for function call ${expr}"
                        case None => errors ++ s"Couldn't figure out arity for function call ${expr}"
                    }
                    (errors, expectedType)
                }
            }

            case Var(name) => if (table.nameDefined(name))
                                (errors, table.getTypeForName(name))
                              else {
                                errors ++ s"Var ${name} not defined before use"
                                (errors, ErrorType)
                              }
            case IntLit(num, width, signed, range) => {
                if (!intOk(num, width, signed, range)) {
                    errors ++ s"${expr} malformed - check width, signedness, and range constraints"
                }

                (errors, IntType)
            }
            case StrLit(value, range) => {rangeOk(range); (errors, StringType)}
            case IntIter(value, range) => {rangeOk(range); (errors, IntIterType)}
            case StrIter(value, range) => {rangeOk(range); (errors, StrIterType)}
        }
    }

    def checkNumericBinOp(e: Expr, l: Expr, r: Expr): Writer[Type] = {
        val (_, left) = checkExpr(l)
        if (left != IntType) errors ++ s"Expected expression of type of int at ${l}"
        val (_, right) = checkExpr(r)
        if (right != IntType) errors ++ s"Expected expression of type of int at ${r}"
        (errors, IntType) 
    }

    def checkStringBinOp(e: Expr, l: Expr, r: Expr): Writer[Type] = {
        val (_, left) = checkExpr(l)
        if (left != StringType) errors ++ s"Expected expression of type of string at ${l}"
        val (_, right) = checkExpr(r)
        if (right != StringType) errors ++ s"Expected expression of type of string at ${r}"

        e match {
            case StrSplit(_, _) => (errors, StrIterType)
            case StrEquals(_, _) => (errors, IntType)
            case _ => (errors, StringType)
        }
    }

    def checkIterBinOp(e: Expr, l: Expr, r: Expr): Writer[Type] = {
        val (_, left) = checkExpr(l)
        if (left != StrIterType && left != IntIterType) errors ++ s"Expected expression of type of iterator at ${l}"
        val (_, right) = checkExpr(r)
        if (right != StrIterType && right != IntIterType) errors ++ s"Expected expression of type of iterator at ${r}"
        if (right != left) errors ++ s"Expected expressions ${l} and ${r} to have same iter type"
        (errors, left) 
    }

    def checkIterUnOp(e: Expr, l: Expr): Writer[Type] = {
        val (_, left) = checkExpr(l)
        if (left != StrIterType && left != IntIterType) errors ++ s"Expected expression of type of iter at ${l}"

        e match {
            case IterLength(_) => (errors, IntType)
            case IterFirst(_) => left match {
                case StrIterType => (errors, StringType)
                case IntIterType => (errors, IntType)
            }
        }
    }

    def checkStringUnop(e: Expr, l: Expr): Writer[Type] = {
        val (_, left) = checkExpr(l)
        if (left != StringType) errors ++ s"Expected expression of type of string at ${l}"
        (errors, IntType) 
    }

    def checkNumericUnOp(e: Expr, l: Expr): Writer[Type] = {
        val (_, left) = checkExpr(l)
        if (left != IntType) errors ++ s"Expected expression of type of int at ${l}"

        e match {
            case Ntos(_) => (errors, StringType)
            case _ => (errors, IntType) 
        }
    }

    def intOk(num: Int, width: Int, signed: Boolean, range: Option[Range]): Boolean = {
        if (!rangeOk(range)) return false

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

        lowerBound <= normalizedNum && normalizedNum <= upperBound
    }

    def rangeOk(range: Option[Range]): Boolean = range match {
        case Some(Range(lo, hi)) => 
            if (lo > hi) {
                errors ++ s"Lower bound on range must be less than or equal to upper bound: ${Range(lo, hi)}"
                false
            } else true
        case None => true
    }

}

object Test {
    import TypeChecker._

    def main(args: Array[String]): Unit = {
        val res = checkFunc(dimacs_scanner)
        println(res)
    }
}