
import Interval._
import AST._
import SymbolTable._
import PropertyStore._

import scala.math.BigInt
import scala.annotation.tailrec

import cats.instances.vector._
import cats.syntax.applicative._ 
import cats.syntax.either._ 
import java.lang.Thread.State

object SafetyChecks {
    type Errors = String
    type VarName = String
    type Result = Either[String, Env]
    type SubrangeConstraint = Option[IntervalLatticeElement]
    type RangeConstraint = Option[IntervalLatticeElement]
    type CurrentValue = Option[IntervalLatticeElement]
    type VarInfo = (VarName, AST.Type, RangeConstraint, SubrangeConstraint, CurrentValue)
    type Env = List[VarInfo]

    var types: SymbolTable = new SymbolTable()

    def rangeToInterval(r: Option[Range]): Option[IntervalLatticeElement] = 
        r.flatMap(x => Some(Interval.fromInts(x.low, x.high)))

    def lookup(v: VarName, env: Env): Either[String, VarInfo] = env match {
        case Nil => Left(s"Couldn't find variable: ${v}")
        case (varname, t, latticeE, olatticeE, value) :: xs => if (v == varname) 
                                                                    Right((varname, t, latticeE, olatticeE, value))
                                                                else
                                                                    lookup(v, xs)
    }

    // TODO: Fix this for hard goal of handling > 1 function. Type
    // checker is already handling this.
    def check(p: Program): Result = {
        types = TypeChecker.getTypes(p)
        val res = checkFunc(p.funcs(0), List())
        println(res)
        res
    }

    def checkArgs(args: List[Arg], env: Env): Result = args match {
        case Nil => Right(env)
        case x :: xs => checkArg(x, env).flatMap(checkArgs(xs, _))
    }

    def checkStmts(stmts: List[Statement], env: Env): Result = stmts match {
        case Nil => Right(env)
        case x :: xs => {
            val parent: Option[Statement] = x match {
                case For(iter1, iter2, acc, body) => Some(x)
                case If(cond, thn, els) => Some(x)
                case _ => None
            }
            checkStmt(x, env, parent).flatMap(checkStmts(xs, _))
        }
    }

    // 1) Add returnType (TODO: fix this to not use hardcoded "returnType") constraint
    //    to environment
    // 2) Add all args constraints to environment
    // 3) Check all statements
    def checkFunc(func: FuncDecl, env: Env): Result = func match {
        case FuncDecl(typ, range, name, args, body) => {
            val funcConstraints = ("returnType", typ, rangeToInterval(range), None, None)
            for {
                funcEnv <- checkArgs(args, funcConstraints :: env)
                resultEnv <- checkStmts(body, funcEnv)
            } yield resultEnv
        }
    }

    // Each arg just needs to be entered into the environment.
    def checkArg(arg: Arg, env: Env): Result = {
        val t = arg.t
        val subrange = arg.subrange
        val range = arg.range
        val name = arg.name

        Right(
            (name, t, rangeToInterval(range), rangeToInterval(subrange), None) :: env
        )
    }

    // Parent is used when in loop or conditional to compute join
    def checkStmt(stmt: Statement, env: Env, parent: Option[Statement]): Result = stmt match {

        // this is horrible
        // Must check that the interval of the result of the expr
        // is contained by the interval and type of the function
        // if the interval of the function is provided.
        case Return(expr) => lookup("returnType", env) match {

            case Right((_, t, rangeConstraint, subRangeConstraint, _)) => rangeConstraint match {

                case None => Right(env)
                case Some(interval) => {

                    checkExpr(expr, env) match {

                        case Right((_, t, range, subrange, value)) => {

                            // Check if the value on the RHS fits in the range of the RHS
                            (value, range) match {
                                case (Some(a), Some(b)) => {
                                    if (!Interval.containedIn(a, b))
                                        return Left(s"Value of ${expr} doesn't fit in needed range: ${range}")
                                }
                                case _ => {}
                            }

                            // Check that the RHS values fit in range constraint of functino return constraints
                            (range, rangeConstraint) match {
                                case (Some(a), Some(b)) => {
                                    if (!Interval.containedIn(a, b))
                                        return Left(s"Value of ${expr} doesn't fit in needed range: ${rangeConstraint}")
                                }
                                case _ => {}
                            }

                            Right(env)
                        }
                        case Left(err) => Left(err)
                    }
                }
            }
            case Left(err) => Left(err)
        }

        
        case Decl(t, name, Some(expr)) => {
            // if Int type, will need to check that the RHS range falls within the Int type bounds
            println(t)
            val optRange: Option[IntervalLatticeElement] = t match {
                case IntDeclType(signed, width) => Some(Interval.fromSignAndWidth(signed,width))
                case _ => None
            }

            checkExpr(expr, env) match {
                // If interval analysis passes on RHS
                case Right((_, _, range, subrange, value)) => {
                    println(s"RHS range: ${range}, RHS value: ${value}")
                    println(s"Opt range: ${optRange}")
                    // Check that the value fits in the range on the RHS
                    (value, range) match {
                        case (Some(interval_a), Some(interval_b)) => {
                            if (!Interval.containedIn(interval_a, interval_b))
                                return Left(s"Value of ${expr} doesn't fit in needed range: ${range}")
                        }
                        case _ => {}
                    }

                    // Check that the RHS range falls in the int type bounds
                    (optRange, range) match {
                        case (Some(l_interval), Some(r_interval)) => {
                            if (!Interval.containedIn(r_interval, l_interval)) {
                                println(s"${l_interval} <=? ${r_interval}")
                                return Left(s"Range of expr ${expr} not within range of ${Decl(t,name,Some(expr))}")
                            }
                        }
                        case _ => {}
                    }

                    // Set the range for this var to the smallest range bound
                    val newVarInfo = (name, t, meet(range, optRange), subrange, value)
                    Right(newVarInfo :: env)
                }

                case Left(err) => Left(err)
            }
        }

        // Shouldn't happen except for inside of forloop (checked
        // during type checking). Add to env, but this will change
        // during loop.
        case Decl(t, name, None) => {
            val newVarInfo = (name, t, None, None, None) // to be changed in loops
            Right(newVarInfo :: env)
        }

        case For(iter1, iter2, acc, body) => checkFor(For(iter1, iter2, acc, body), env)

        // TODO: Do if
        case _ => Right(env)
        // for does all checks linearly, merge with original env, rerun for with check again
    }

    def checkFor(f: For, env: Env): Result = {
        val idxName = f.iter1.idx
        val idxType = types.getTypeForName(idxName)
        val iter = f.iter1.iterator

        val envWithIdx = for {
            iteratorInfo <- checkExpr(iter, env)
            idxInfo <- Right((idxName, idxType, iteratorInfo._4, None, iteratorInfo._4))
        } yield (idxName, idxType, iteratorInfo._4, None, iteratorInfo._4) :: env

        val envWithAcc = for {
            origEnv <- envWithIdx
            newEnv <- checkStmt(f.acc, origEnv, Some(f))
        } yield newEnv

        println(envWithAcc)
        envWithAcc
    }

    // Just returning a nameless VarInfo tuple with interval, range, and subrange
    // data. The interval should be (and is checked to be) contained within the
    // range or subrange. For example, IntLit(100, 32, true, Some(Range(0, 100)))
    // would pass checking since 100 is from [0,100].meet(int32interval). The function
    // would then return ArgInfo = ("", IntType, Interval(-2^31, 2^31-1), None, [100, 100]).
    def checkExpr(expr: Expr, env: Env): Either[Errors, VarInfo] = expr match {
        case s : StrLit => strLitToInterval(s)
        case i : IntLit => intLitToInterval(i)
        case iit : IntIter => intIterToInterval(iit)
        case sit : StrIter => strIterToInterval(sit)
        case Add(left, right) => 
            for {
                lhs <- checkExpr(left, env)
                rhs <- checkExpr(right,env)
                exprRes <- Right(("", IntType, Interval.opt_join(lhs._3, rhs._3), Interval.opt_join(lhs._4, rhs._4), Interval.binop(Interval.plus, lhs._5, rhs._5)))
                res <- if (!Interval.containedIn(exprRes._5, exprRes._3))
                            Left(s"${expr} doesn't fit in expected range: ${exprRes._3}")
                        else
                            Right(exprRes)
            } yield res
        case Minus(left, right) => 
            for {
                lhs <- checkExpr(left, env)
                rhs <- checkExpr(right,env)
                exprRes <- Right(("", IntType, Interval.opt_join(lhs._3, rhs._3), Interval.opt_join(lhs._4, rhs._4), Interval.binop(Interval.minus, lhs._5, rhs._5)))
                res <- if (!Interval.containedIn(exprRes._5, exprRes._3))
                            Left(s"${expr} doesn't fit in expected range: ${exprRes._3}")
                        else
                            Right(exprRes)
            } yield res
        case Mult(left, right) => 
            for {
                lhs <- checkExpr(left, env)
                rhs <- checkExpr(right,env)
                exprRes <- Right(("", IntType, Interval.opt_join(lhs._3, rhs._3), Interval.opt_join(lhs._4, rhs._4), Interval.binop(Interval.mult, lhs._5, rhs._5)))
                res <- if (!Interval.containedIn(exprRes._5, exprRes._3))
                            Left(s"${expr} doesn't fit in expected range: ${exprRes._3}")
                        else
                            Right(exprRes)
            } yield res
        case Div(left, right) => 
            for {
                lhs <- checkExpr(left, env)
                rhs <- checkExpr(right,env)
                exprRes <- Right(("", IntType, Interval.opt_join(lhs._3, rhs._3), Interval.opt_join(lhs._4, rhs._4), Interval.binop(Interval.div, lhs._5, rhs._5)))
                res <- if (!Interval.containedIn(exprRes._5, exprRes._3))
                            Left(s"${expr} doesn't fit in expected range: ${exprRes._3}")
                        else
                            Right(exprRes)
            } yield res
        case Eq(left, right) => Right(("", IntType, Some(Interval.fromInts(0, 1)), None, Some(Interval.fromInts(0, 1))))
        case Lte(left, right) => Right(("", IntType, Some(Interval.fromInts(0, 1)), None, Some(Interval.fromInts(0, 1))))
        case Lt(left, right) => Right(("", IntType, Some(Interval.fromInts(0, 1)), None, Some(Interval.fromInts(0, 1))))
        case Gte(left, right) => Right(("", IntType, Some(Interval.fromInts(0, 1)), None, Some(Interval.fromInts(0, 1))))
        case Gt(left, right) => Right(("", IntType, Some(Interval.fromInts(0, 1)), None, Some(Interval.fromInts(0, 1))))
        case Neq(left, right) => Right(("", IntType, Some(Interval.fromInts(0, 1)), None, Some(Interval.fromInts(0, 1))))

        case Or(left, right) => Right(("", IntType, Some(Interval.fromInts(0, 1)), None, Some(Interval.fromInts(0, 1))))
        case And(left, right) => Right(("", IntType, Some(Interval.fromInts(0, 1)), None, Some(Interval.fromInts(0, 1))))

        case Not(left) => Right(("", IntType, Some(Interval.fromInts(0, 1)), None, Some(Interval.fromInts(0, 1))))

        case Ntos(left) => checkExpr(left, env) match {
            case Right((_, t, range, subrange, value)) => {
                
                (range, value) match {
                    case (Some(a), Some(b)) => {
                        if (!Interval.containedIn(b, a))
                            Left(s"${left} doesn't fit in expected range: ${range}")
                    }
                    case _ => {}
                }

                val stringRange = value match {
                    case Some(Interval(lo, hi)) => Some(Interval.fromBigInts(0, digitsInBigInt(lo).max(digitsInBigInt(hi))))
                    case _ => None
                }

                Right(("", StringType, stringRange, None, None))
            }
            case Left(err) => Left(err)
        }

        case Ston(left) => checkExpr(left, env) match {
            case Right((_, t, range, subrange, value)) => {

                val stringRange = value match {
                    case Some(Interval(lo, hi)) => Some(Interval.fromBigInts(0, digitsInBigInt(lo).max(digitsInBigInt(hi))))
                    case _ => None
                }

                val intRange = range match {
                    case None => Some(Interval.intervalTop)
                    case Some(intrvl) => intrvl match {
                        case Interval(lo, hi) => Some(Interval.fromBigInts(stringLengthToLowDigit(lo), stringLengthToHighDigit(hi)))
                        case Bottom => Some(Interval.intervalTop)
                    }
                }

                Right(("", IntType, intRange, None, intRange))
            }
            case Left(err) => Left(err)
        }

        case IterLength(left) => checkExpr(left, env) match {
            case Right((_, t, range, subrange, value)) => range match {
                case None => Right(("", IntType, Some(Interval.defaultLengthInterval), subrange, value))
                case Some(r) => Right(("", t, range, subrange, value))
            }
            case Left(err) => Left(err)
        }

        // Could be more precise
        case StrSplit(left, right) =>
            for {
                targetStrInfo <- checkExpr(left, env)
                iteratorInfo <- Right(("", StrIterType, targetStrInfo._3, targetStrInfo._3, None))
            } yield iteratorInfo

        case Var(name) => lookup(name, env).flatMap(varinfo => Right(varinfo))

        case _ => Left(s"Checking of ${expr} not yet supported")
    }

    def castSafe(fromSigned: Boolean, fromWidth: Int, fromRange: Option[IntervalLatticeElement], toSigned: Boolean, toWidth: Int): Boolean = {
        if (fromSigned && !toSigned) {
            fromRange match {
                case Some(valRange) => 
                    Interval.containedIn(valRange, Interval.fromSignAndWidth(toSigned, toWidth))
                // Assume that all values from ${fromSigned}int${fromWidth}_t
                case None => false
            }
        } else if (!fromSigned && toSigned) {
            fromRange match {
                case Some(valRange) => 
                    Interval.containedIn(valRange, Interval.fromSignAndWidth(toSigned, toWidth))
                case None => (fromWidth < toWidth) // e.g. uint8_t can fit in int16_t, ...
            }
        } else {
            val defaultCheck = fromWidth <= toWidth
            val rangeCheck = fromRange match {
                case Some(valRange) => 
                    Interval.containedIn(valRange, Interval.fromSignAndWidth(toSigned, toWidth))
                case None => false
            }
            defaultCheck || rangeCheck
        }
        false
    }

    def intIterToInterval(iit: IntIter): Either[Errors, VarInfo] =
        Right(
            ("", IntIterType, rangeToInterval(iit.range), rangeToInterval(iit.subrange), None)
        )

    def strIterToInterval(iit: StrIter): Either[Errors, VarInfo] =
        Right(
            ("", StrIterType, rangeToInterval(iit.range), rangeToInterval(iit.subrange), None)
        )

    def strLitToInterval(str: StrLit): Either[Errors, VarInfo] = str.range match {
        case Some(Range(lo, hi)) => {
            if (str.value.length > hi)
                Left(s"String ${str.value} longer than length constraint: [${lo}, ${hi}]")
            else
                Right(
                    ("", StringType, rangeToInterval(str.range), None, Some(Interval.fromInt(str.value.length)))
                )
        }
        case None => Right(
            ("", StringType, Some(Interval.fromInt(str.value.length)), None, Some(Interval.fromInt(str.value.length)))
        )
    }

    def intLitToInterval(int: IntLit): Either[Errors, VarInfo] = {
        val n = int.num
        val width = int.width
        val signed = int.signed
        val range = int.range

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

        Right(
            ("", IntType, Some(Interval.fromBigInts(lowerBound, upperBound)), None, Some(Interval.fromInt(n)))
        )
    }

    // include unary minus sign
    def digitsInBigInt(num: BigInt): BigInt =
        if (num.equals(BigInt(0)))
            0
        else if (num < BigInt(0))
            1 + digitsInBigInt(num.abs)
        else
            1 + digitsInBigInt(num / BigInt(10))

    def stringLengthToLowDigit(len: BigInt, tmp: String = ""): BigInt =
        if (tmp.length() == len - 1)
            BigInt(("-" ++ tmp).toInt)
        else
            stringLengthToLowDigit(len, tmp ++ "9")

    def stringLengthToHighDigit(len: BigInt, tmp: String = ""): BigInt =
        if (tmp.length() == len)
            BigInt(tmp.toInt)
        else
            stringLengthToHighDigit(len, tmp ++ "9")

}



object SafetyChecksTest {
    import SafetyChecks._ 
    import AST._
    import ExamplePrograms._

    def main(args: Array[String]): Unit = {
        val res = check(array_average)
        println(res)
    }
}