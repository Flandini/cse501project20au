
import AST._
import javax.sql.StatementEvent

object CodeGen {
    type Code = String

    def genHeader: Code = """
        |#include <string.h>
        |#include <stdlib.h>
        |#include <ctype.h>
        |#include <stdint.h>
    """.stripMargin

    def genPreconditions: Code = """"""

    def genStmt(stmt: Statement): Code = stmt match {
        case Decl(t, name, expr) => ""
        case For(iter1, iter2, acc, body) => ""
        case If(cond, thn, els) => ""
        case Return(expr) => s"return ${genExpr(expr)};"
    }

    def genType(t: Type): Code = t match {
        case StringType => "char*"
        case IntType => "int" // TODO: ??
        case StrIterType => "char**"
        case IntIterType => "int*"
        case ProgramType => "" // Shouldn't propagate to here
        case ErrorType => "" // Shouldn't propagate to here
    }

    def genFunc(f: FuncDecl): Code = {
        ""
    }

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
            case StrLength(l) => "" // TODO: Custom gen method

            case IterConcat(l,r) => "" // TODO: Custom gen method
            case IterFirst(l) => "" // TODO: Custom gen method
            case IterLength(l) => "" // TODO: Custom gen method

            case FunCall(name, params) => name + "(" + params.map(genExpr).mkString(", ") + ")"
            case Var(name) => name

            case IntLit(num, width, signed, range) => num.toString
            case StrLit(value, range) => value
            case IntIter(value, range) => "" // TODO: ?
            case StrIter(value, range) => "" // TODO: ?
        }
    }
}

object CodegenTest {
    import CodeGen._

    def main(args: Array[String]): Unit = {
        println("")
    }
}