
import AST._
import SafetyChecks._
import Interval._

object CodeGen {
    type Code = String
    type Range = (Option[IntervalLatticeElement], Option[IntervalLatticeElement])

    var constraintData: SafetyChecks.Env = List[VarInfo]()

    def lookup(v: VarName, env: Env): Range = env match {
        case Nil => (None, None)
        case (varname, t, latticeE, olatticeE, value) :: xs => if (v == varname) 
                                                                    (latticeE, olatticeE)
                                                                else
                                                                    lookup(v, xs)
    }

    def genHeader: Code = """
        |#include <string.h>
        |#include <stdlib.h>
        |#include <ctype.h>
        |#include <stdint.h>
    """.stripMargin ++ ("\n" * 2)

    def genPreconditions: Code = """"""

    def genStmt(stmt: Statement, indent: Int = 1): Code = {
        val prependStr = " " * 4 * indent
        val stmtStr = stmt match {
            case Decl(t, name, expr) => expr match {
                case None => s" ${genType(t)} ${name} "
                case Some(e) =>  s"${genType(t)} ${name} = ${genExpr(e)};\n"
            }
            case For(iter1, iter2, acc, body) => genForStmt(For(iter1, iter2, acc, body), indent)
            case If(cond, thn, els) => genIfStmt(If(cond, thn, els), indent)
            case Return(expr) => s"return ${genExpr(expr)};\n"
        }
        prependStr ++ stmtStr
    }

    def forIterNeedsExtract(iter: Iterator): Boolean = exprIsVarName(iter.iterator)
    def genForStmt(f: For, indent: Int = 1): Code = {
        //val decl
        ""
    }

    def genIfStmt(ifStmt: If, indent: Int = 1): Code = {
        val prependStr = " " * 4 * indent
        val check = s"if ( ${genExpr(ifStmt.cond)} ) {\n"
        val thenBody = ifStmt.thn.map(genStmt(_, indent + 1)).mkString
        val thenClose = "}"
        val elsePart: Code = ifStmt.els match {
            case None => ""
            case Some(stmts) => {
                val elsePart = s" else {\n"
                val elseBody = stmts.map(genStmt(_, indent + 1)).mkString
                val elseClose = s"}"
                elsePart ++ elseBody ++ prependStr ++ elseClose
            }
        }
        prependStr ++ check ++ thenBody ++ prependStr ++ thenClose ++ elsePart
    }

    def genType(t: Type): Code = t match {
        case StringType => "char*"
        case IntType => "int" // TODO: ??
        case StrIterType => "char**"
        case IntIterType => "int*"
        case IntDeclType(signed: Boolean, width: Int) => {
            val sign = if (signed) "" else "u"
            s"${sign}int${width}"
        }
        case ProgramType => "" // Shouldn't propagate to here
        case ErrorType => "" // Shouldn't propagate to here
    }

    def genProg(p: Program): Code = {
        val rangeData = SafetyChecks.check(p)
        rangeData match {
            case Left(err) => {} // Should work by here or safety and type checks would've failed
            case Right(constraintTable) => constraintData = constraintTable
        }
        val header = genHeader
        val funcs = genFunc(p.funcs(0))
        header ++ funcs
    }

    def genFunc(f: FuncDecl): Code = f match {
        case FuncDecl(t, range, name, args, body) => {
            val typeStr = genType(t)
            val argslist = args.map(genArg).mkString(", ")
            val prologue = s"${typeStr} ${name}(${argslist}) {"
            val statementsStr = body.map(genStmt(_)).mkString
            val epilogue = "}"
            prologue ++ "\n" ++ statementsStr ++ epilogue ++ "\n\n"
        }
    }

    def genArg(a: Arg): Code = genType(a.t) ++ " " ++ a.name

    def genExpr(expr: Expr): Code = {
        expr match {
            case Add(l, r) => s"${genExpr(l)} + ${genExpr(r)}"
            case Minus(l,r) => s"${genExpr(l)} - ${genExpr(r)}"
            case Mult(l,r) => s"${genExpr(l)} * ${genExpr(r)}"
            case Div(l,r) => s"${genExpr(l)} / ${genExpr(r)}"
            case Eq(l,r) => s"${genExpr(l)} == ${genExpr(r)}"
            case Neq(l,r) => s"${genExpr(l)} != ${genExpr(r)}"
            case Gte(l,r) => s"${genExpr(l)} >= ${genExpr(r)}"
            case Lte(l,r) => s"${genExpr(l)} <= ${genExpr(r)}"
            case Gt(l,r) => s"${genExpr(l)} > ${genExpr(r)}"
            case Lt(l, r) => s"${genExpr(l)} < ${genExpr(r)}"
            case Or(l,r) => s"${genExpr(l)} || ${genExpr(r)}"
            case And(l, r) => s"${genExpr(l)} && ${genExpr(r)}"
            case Not(l) => s"!(${genExpr(l)})"
            case Ntos(l) => s"itoa(${genExpr(l)})"

            case Ston(l) => s"atoi(${genExpr(l)})"
            case StrSplit(l,r) => "" // TODO: Custom gen method
            case StrEquals(l, r) => "" // TODO: Custom gen method
            case StrConcat(l, r) => "" // TODO: Custom gen method
            case StrAppend(l, r) => "" // TODO: Custom gen method
            case StrLength(l) => s"strlen(${genExpr(l)})" // TODO: Custom gen method

            // need range/constraint info from typechecking/safetychecks
            case IterConcat(l,r) => "" // TODO: Custom gen method
            case IterFirst(l) => s"${genExpr(l)}[0]" // TODO: Custom gen method
            case IterLength(l) => "" // TODO: Custom gen method
            }

            case FunCall(name, params) => name + "(" + params.map(genExpr).mkString(",") + ")"
            case Var(name) => name

            case IntLit(num, width, signed, range) => num.toString
            case StrLit(value, range) => value
            case IntIter(value, range, subrange) => "" // should never be propagated in code generation
            case StrIter(value, range, subrange) => "" // should never be propagated in code generation
        }
    }

    def exprIsVarName(expr: Expr): Boolean = expr match {
        case Var(name) => true 
        case _ => false
    }
}

object CodegenTest {
    import CodeGen._
    import ExamplePrograms._

    def main(args: Array[String]): Unit = {
        println(genProg(one_decl_one_return))
        //println("*" * 40)
        //println(genProg(array_average))
    }
}