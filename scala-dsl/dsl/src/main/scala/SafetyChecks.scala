
import Interval._
import AST._

import scala.math.BigInt
import scala.annotation.tailrec

import cats.instances.vector._
import cats.syntax.applicative._ 
import cats.syntax.either._ 

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

    def check(p: Program): Result = checkFunc(p.funcs(0), List())

    def checkArgs(args: List[Arg], env: Env): Result = args match {
        case Nil => Right(env)
        case x :: xs => checkArg(x, env).flatMap(res => checkArgs(xs, res ++ env))
    }

    def checkFunc(func: FuncDecl, env: Env): Result = func match {
        case FuncDecl(typ, range, name, args, body) =>  
            val funcConstraints = ("returnType", typ, rangeToInterval(range), None, None)
            checkArgs(args, funcConstraints :: env)
    }

    def checkArg(arg: Arg, env: Env): Result = {
        val t = arg.t
        val subrange = arg.subrange
        val range = arg.range
        val name = arg.name

        Right(
            (name, t, rangeToInterval(range), rangeToInterval(subrange), None) :: env
        )
    }

    def checkStmt(stmt: Statement, env: Env): Result = stmt match {

        // this is horrible
        case Return(expr) => lookup("returnType", env) match {
            case Right((_, t, rangeConstraint, _, _)) => rangeConstraint match {
                case None => Right(env)
                case Some(interval) => {
                    checkExpr(expr, env) match {
                        case Right((_, t, range, subrange, value)) => {
                            value match {
                                case Some(value) => {
                                    if (Interval.lte(value, interval))
                                        Right(env)
                                    else
                                        Left(s"${expr} does not fit in range constraint of ${interval}")
                                }
                                case None => Right(env)
                            }
                        }
                        case Left(err) => Left(err)
                    }
                }
            }
        }

        case Decl(t, name, Some(expr)) => {
            checkExpr(expr, env) match {
                case Right((_, _, range, subrange, value)) => {
                    val newVarInfo = (name, t, range, subrange, value)
                    Right(newVarInfo :: env)
                }
                case Left(err) => Left(err)
            }
        }

        case Decl(t, name, None) => {
            Right(env)
        }

        // TODO: Do if and for
    }

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

    def main(args: Array[String]): Unit = {
        val res = check(array_average)
        println(res)
    }
}