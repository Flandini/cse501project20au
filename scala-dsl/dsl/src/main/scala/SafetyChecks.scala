
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
    type VarInfo = (VarName, AST.Type, Option[IntervalLatticeElement], Option[IntervalLatticeElement])
    type Env = List[VarInfo]

    def rangeToInterval(r: Option[Range]): Option[IntervalLatticeElement] = 
        r.flatMap(x => Some(Interval.fromInts(x.low, x.high)))

    def lookup(v: VarName, env: Env): Either[String, VarInfo] = env match {
        case Nil => Left(s"Couldn't find variable: ${v}")
        case (varname, t, latticeE, olatticeE) :: xs => if (v == varname) 
                                                            Right((varname, t, latticeE, olatticeE))
                                                        else
                                                            lookup(v, xs)
    }

    def check(p: Program): Unit = checkFunc(p.funcs(0), List())

    def checkFunc(func: FuncDecl, env: Env): Unit = func match {
                case FuncDecl(typ, range, name, args, body) =>  
                    args.foreach(arg => println(checkArg(arg, env)))
            }

    def checkArg(arg: Arg, env: Env): Result = {
        val t = arg.t
        val subrange = arg.subrange
        val range = arg.range
        val name = arg.name

        Right(
            (name, t, rangeToInterval(range), rangeToInterval(subrange)) :: env
        )
    }
}

object SafetyChecksTest {
    import SafetyChecks._ 
    import AST._

    def main(args: Array[String]): Unit = {
        val res = check(array_average)
    }
}