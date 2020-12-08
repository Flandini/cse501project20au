
import Interval._
import AST._

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
    def check(p: Program): Result = checkFunc(p.funcs(0), List())

    def checkArgs(args: List[Arg], env: Env): Result = args match {
        case Nil => Right(env)
        case x :: xs => checkArg(x, env).flatMap(res => checkArgs(xs, res ++ env))
    }

    def checkStmts(stmts: List[Statement], env: Env): Result = stmts match {
        case Nil => Right(env)
        case x :: xs => {
            val parent: Option[Statement] = x match {
                case For(iter1, iter2, acc, body) => Some(x)
                case If(cond, thn, els) => Some(x)
                case _ => None
            }
            checkStmt(x, env, parent).flatMap(res => checkStmts(xs, res ++ env))
        }
    }

    // 1) Add returnType (TODO: fix this to not use hardcoded "returnType") constraint
    //    to environment
    // 2) Add all args constraints to environment
    // 3) Check all statements
    def checkFunc(func: FuncDecl, env: Env): Result = func match {
        case FuncDecl(typ, range, name, args, body) =>  
            val funcConstraints = ("returnType", typ, rangeToInterval(range), None, None)
            for {
                funcEnv <- checkArgs(args, funcConstraints :: env)
                resultEnv <- checkStmts(body, funcEnv)
            } yield resultEnv
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
            val optRange: Option[IntervalLatticeElement] = t match {
                case IntDeclType(signed, width) => Some(Interval.fromSignAndWidth(signed,width))
                case _ => None
            }

            checkExpr(expr, env) match {
                // If interval analysis passes on RHS
                case Right((_, _, range, subrange, value)) => {

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
                            if (!Interval.containedIn(r_interval, l_interval))
                                return Left(s"Range of expr ${expr} not within range of ${Decl(t,name,Some(expr))}")
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

        // TODO: Do if and for
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
        case Var(name) => lookup(name, env).flatMap(varinfo => Right(varinfo))
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

        Right(
            ("", IntType, Some(Interval.fromBigInts(lowerBound, upperBound)), None, Some(Interval.fromInt(n)))
        )
    }
}



object SafetyChecksTest {
    import SafetyChecks._ 
    import AST._
    import ExamplePrograms._

    def main(args: Array[String]): Unit = {
        val res = check(one_decl_one_return)
        println(res)
    }
}