
import AST._ 
import SymbolTable._
import PropertyStore._

import scala.collection.mutable.Stack
import scala.collection.immutable.Vector
import scala.collection.mutable.ListBuffer
import scala.math.BigInt
import scala.annotation.tailrec

import cats.instances.vector._
import cats.syntax.applicative._ 
import cats.syntax.either._ 

object TypeChecker {
    type Error = String
    type Result = Either[Error, Type]
    type FunctionName = String

    // Wrapper around String => String => Property
    var table: SymbolTable = new SymbolTable()

    // For checking return types, recursion
    var funcStack: Stack[FunctionName] = Stack()

    def check(prog: Program): Result =
        for {
            firstPassCheck <- shortCircuitListCheck(prog.funcs, checkFuncSignature)
            secondPassCheck <- shortCircuitListCheck(prog.funcs, checkFuncBody)
        } yield secondPassCheck

    def getTypes(prog: Program): SymbolTable = {
        check(prog)
        table
    }

    @tailrec
    def shortCircuitListCheck[A](lst: List[A], f: A => Result): Result = lst match {
        case Nil => Right(ProgramType)
        case x :: Nil => f(x)
        case x :: xs => f(x) match {
            case Left(err) => Left(err)
            case Right(t) => shortCircuitListCheck(xs, f)
        }
    }
    def checkStmts(stmts: List[Statement]): Result = shortCircuitListCheck(stmts, checkStmt)
    def checkArgs(args: List[Arg]): Result = shortCircuitListCheck(args, checkArg)
    def checkAllDeclsHaveRhs(stmts: List[Statement]): Result = shortCircuitListCheck(stmts, checkDeclHasRhs)

    def checkFuncSignature(func: FuncDecl): Result = func match {
            case FuncDecl(typ, range, name, args, body) => {
                val p = PropertyStore(name)
                            .setRange(range)
                            .setType(typ)
                            .setArity(args.length)

                if (table.nameDefined(name))
                    return Left(s"Func ${name} already defined")

                table.setPropertiesForName(name, p)

                for {
                    argsT <- checkArgs(args)
                    declsT <- checkAllDeclsHaveRhs(body)
                } yield typ
            }
        }

    def checkFuncBody(func: FuncDecl): Result =
        func match {
            case FuncDecl(typ, range, name, args, body) => {
                funcStack.push(name)
                val res = checkStmts(body)
                funcStack.pop()
                res
            }
        }
        
    def checkStmt(stmt: Statement): Result =
        stmt match {
            case Return(expr) => checkExpr(expr) match {
                    case Left(err) => Left(err)
                    case Right(t) => {
                        val currentFunc = funcStack.top
                        val expectedReturnType = table.getPropertyForName(currentFunc, "type")

                        expectedReturnType match {
                            case None => Left(s"Couldn't infer return type for function: ${funcStack.top}")
                            case Some(retType) => if (retType !~ t)
                                                    Left(s"Return type of ${expr} doesnt match expected ${retType}")
                                                  else
                                                    Right(t)

                        }
                    }
                }

            case For(iter1, iter2, acc, body) => {
                val iter2type = iter2 match {
                    case Some(iter) => checkIterator(iter)
                    case None => Right(IntType) //placeholder, doesn't matter
                }

                for {
                    iter1t <- checkIterator(iter1)
                    iter2t <- iter2type
                    acct <- checkStmt(acc)
                    bodyT <- checkLoopBody(body)
                } yield bodyT
            }

            case If(cond, thn, els) => {
                val thnType = for {
                    condType <- checkExpr(cond)
                    res <- if (condType != IntType) Left(s"Expected int type in if condition ${cond}")
                           else Right(condType)
                    thnType <- checkStmts(thn)
                } yield thnType

                thnType match {
                    case Left(err) => Left(err)
                    case Right(t) => els match {
                        case None => Right(t)
                        case Some(stmts) => checkStmts(stmts)
                    }
                }
            }

            case Decl(t, name, expr) => {
                if (table.nameDefined(name))
                    return Left(s"Cannot redefine ${name} at ${Decl(t, name, expr)}")

                val rhsType = expr match {
                    case None => Right(t)
                    case Some(rhs) => checkExpr(rhs) match {
                        case Left(err) => return Left(err)
                        case Right(t) => Right(t)
                    }
                }

                val p = PropertyStore(name).setType(t)
                table.setPropertiesForName(name, p)

                Right(t)
            }
        }

    @tailrec
    def checkLoopBody(body: List[ForBody]): Result = body match {
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

    def checkIterator(iter: Iterator): Result = 
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

    def checkArg(arg: Arg): Result = {
        val typ = arg.t
        val subrange = arg.subrange
        val range = arg.range
        val name = arg.name

        subrange match {
            case Some(range) =>
                if (typ == IntType || typ == StringType)
                    return Left(s"${name} is a ${typ} which cannot have a subrange")
            case None => {}
        }

        val p = PropertyStore(name)
                    .setType(typ)
                    .setRange(range)
                    .setSubrange(subrange)

        if (table.nameDefined(name))
            return Left(s"Arg name${name} already previously defined")

        table.setPropertiesForName(name, p)

        Right(typ)
    }

    // TODO: Change AST to avoid all the cases
    def checkExpr(expr: Expr): Either[Error, Type] = {
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
                else if (funcStack.top == name)
                    Left(s"Recursion is not allowed.")
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
                              else if (name.length == 0) Left("Empty var names not allowed")
                              else Left(s"Var ${name} not defined before use")
            case IntLit(num, width, signed, range) => 
                for {
                    rangeCheck <- rangeOk(range)
                    intCheck <- intOk(num, width, signed, range)
                    retType <- if (intCheck) Right(IntType)
                               else Left(s"Int lit ${IntLit(num, width, signed, range)} invalid")
                } yield retType
            case StrLit(value, range) => rangeOk(range).flatMap(_ => Right(StringType))
            case IntIter(value, range, subrange) => rangeOk(range).flatMap(_ => Right(IntIterType))
            case StrIter(value, range, subrange) => rangeOk(range).flatMap(_ => Right(StrIterType))
        }
    }

    def checkNumericBinOp(e: Expr, l: Expr, r: Expr): Result = 
        for {
            ltype <- checkExpr(l) 
            _ <- if (ltype !~ IntType) Left(s"Expected expression of type of int at ${l}")
                 else Right(IntType)
            rtype <- checkExpr(r)
            _ <- if (rtype !~ IntType) Left(s"Expected expression of type of int at ${r}")
                 else Right(IntType)
        } yield IntType

    def checkStringBinOp(e: Expr, l: Expr, r: Expr): Result = {
        val returnType = e match {
            case StrSplit(_, _) => StrIterType
            case StrEquals(_, _) => IntType
            case _ => StringType
        }
        for {
            ltype <- checkExpr(l)
            _ <- if (ltype !~ StringType) Left(s"Expected expression of type of string at ${l}")
                 else Right(StringType)
            rtype <- checkExpr(r)
            _ <- if (rtype !~ StringType) Left(s"Expected expression of type of string at ${r}")
                 else Right(StringType)
        } yield returnType
    }

    def checkIterBinOp(e: Expr, l: Expr, r: Expr): Result = 
        for {
            ltype <- checkExpr(l)
            _ <- if (ltype !~ StrIterType && ltype !~ IntIterType) Left(s"Expected expression of type of iterator at ${l}")
                 else Right(IntIterType) // intermediate, doesn't matter
            rtype <- checkExpr(r)
            _ <- if (rtype !~ StrIterType && rtype !~ IntIterType) Left(s"Expected expression of type of iterator at ${r}")
                 else Right(IntIterType) // intermediate, doesn't matter
            returnType <- if (ltype !~ rtype) Left(s"Expected expressions ${l} and ${r} to have same iter type")
                          else Right(ltype)
        } yield returnType 
 
    def checkIterUnOp(e: Expr, l: Expr): Result = 
        for {
            ltype <- checkExpr(l)
            check <- if (ltype !~ StrIterType && ltype !~ IntIterType) Left(s"Expected expression of type of iter at ${l}")
                     else Right(ltype)
            returnType <- inferIterUnOpType(e, ltype)
        } yield returnType

    def inferIterUnOpType(parentExp: Expr, leftType: Type): Result = parentExp match {
        case IterLength(_) => Right(IntType)
        case IterFirst(_) => leftType match {
            case StrIterType => Right(StringType)
            case IntIterType => Right(IntType)
            case _ => Left(s"iter_first requires an iterator as its argument")
        }
    }

    def checkStringUnop(e: Expr, l: Expr): Result =
        for {
            ltype <- checkExpr(l)
            check <- if (ltype !~ StringType) Left(s"Expected expression of type of string at ${l}")
                     else Right(ltype)
        } yield check

    def checkNumericUnOp(e: Expr, l: Expr): Result = {
        val returnType = e match {
            case Ntos(_) => StringType
            case _ => IntType
        }
        for {
            ltype <- checkExpr(l)
            check <- if (ltype !~ IntType) Left(s"Expected expression of type of int at ${l}")
                     else Right(ltype)
        } yield returnType
    }

    def intOk(num: Int, width: Int, signed: Boolean, range: Option[Range]): Either[Error, Boolean] = {
        var lowerBound = if (signed) -BigInt(2).pow(width - 1) else BigInt(0)
        var upperBound = if (signed) BigInt(2).pow(width - 1) - BigInt(1) else BigInt(2).pow(width) - BigInt(1)

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

    def rangeOk(range: Option[Range]): Either[Error, Boolean] = range match {
        case Some(Range(lo, hi)) => 
            if (lo > hi) Left(s"Lower bound on range must be less than or equal to upper bound: ${Range(lo, hi)}")
            else Right(true)
        case None => Right(true)
    }

    def checkDeclHasRhs(stmt: Statement): Result = stmt match {
        case Decl(t, name, expr) => expr match {
            case None => Left(s"Declaration of var ${name} much have a rhs")
            case Some(_) => Right(ProgramType)
        }
        case _ => Right(ProgramType)
    }

    implicit class TypeSyntax(t: Type) {
        def ~(other: Type): Boolean = t match {
            case StringType => other == StringType
            case IntType => 
                other.isInstanceOf[IntDeclType] || other == IntType
            case IntIterType =>  other == IntIterType
            case StrIterType =>  other == StrIterType
            case ProgramType =>  other == ProgramType
            case ErrorType => false
            case IntDeclType(signed, width) => 
                other.isInstanceOf[IntDeclType] || other == IntType
        }

        def !~(other: Type): Boolean = !(t ~ other)
    }

    implicit class PropertySyntax(p: Property) {
        def ~(other: Property): Boolean = p match {
            case t: Type => 
                other match {
                    case o: Type => t ~ o
                    case _ => false
                }
            case _ => false
        }

        def !~(other: Property): Boolean = !(p ~ other)
    }
}

object TypecheckTest {
    import TypeChecker._
    import ExamplePrograms._

    def main(args: Array[String]): Unit = {
        val res = check(dimacs_scanner)
        println(res)
        val res2 = check(array_average)
        println(res2)
    }
}