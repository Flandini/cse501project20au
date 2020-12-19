import AST._

package syntax {

object Signedness {
    def signed: Boolean = true
    def unsigned: Boolean = false
}

case class Argument(name: String) {
    private var t: TypeSyntax = ErrorT
    private var range: Option[Range] = None
    private var subrange: Option[Range] = None

    def withType(typ: TypeSyntax): Argument = {
        t = typ
        this
    }

    def withRange(r: Range): Argument = {
        range = Some(r)
        this
    }

    def withSubrange(r: Range): Argument = {
        subrange = Some(r)
        this
    }

    def toAstArg: Arg = Arg(TypeSyntax.toAstType(t), subrange, range, name)
}

case class Variable(name: String) {
    private var t: Option[TypeSyntax] = None
    private var v: Option[Expr] = None

    def withType(typ: TypeSyntax): Variable = {
        t = Some(typ)
        this
    }

    def withValue(expr: Expr): Variable = {
        v = Some(expr)
        this
    }

    def toDecl: Decl = {
        val declType: TypeSyntax = t match {
            case Some(typ) => typ
            case None => ErrorT // Let type checker complain
        }
        Decl(TypeSyntax.toAstType(declType), name, v)
    }
}

//     case class For(iter1: Iterator, iter2: Option[Iterator], acc: Decl, body: List[ForBody]) extends Statement
case class ForEach(idx: String) {
    //var iterExpr: Option[Expr] = None
    var iterExpr: Expr = Var("")
    var iterVar: Variable = Variable("")
    var acc: Variable = Variable("")
    var body: List[Expr] = List()

    var isIterExpr: Boolean = false 
    var isIterVar: Boolean = false

    def inExpr(expr: Expr): ForEach = {
        iterExpr = expr
        isIterExpr = true
        isIterVar = false
        this
    }

    def inVar(v: Variable): ForEach = {
        iterVar = v
        isIterVar = true
        isIterExpr = false
        this
    }

    def withAcc(v: Variable): ForEach = {
        acc = v
        this
    }

    def setAcc(expr: Expr): ForEach = {
        body ::= expr
        this
    }

    def startFor(): ForEach = this
    def endFor: For = {
        val iter: Expr = if (isIterExpr) 
                            iterExpr 
                         else 
                            Var(iterVar.name)
        For(Iterator(idx, iter), None, acc.toDecl, body.reverse)
    }
}

case class Function(name: String) {
    import CodeGen._

    private var args: List[Arg] = List()
    private var body: List[Statement] = List()
    private var returnValRange: Option[AST.Range] = None
    private var returnT: TypeSyntax = ErrorT

    def bound(lo: Int, hi: Int): Function = {
        returnValRange = Some(AST.Range(lo, hi))
        this
    }

    def returnType(t: TypeSyntax): Function = {
        returnT = t
        this
    }

    def arg(a: Argument): Function = {
        args ::= a.toAstArg
        this
    }

    def declare(v: Variable): Function = {
        body ::= v.toDecl
        this
    }

    def ret(expr: Expr): Function = {
        body ::= Return(expr)
        this
    }

    def loop(f: For): Function = {
        body ::= f
        this
    }

    def startFunction(): Function = this

    def endFunction: Code =
        genProg(AST.Program(List(
            AST.FuncDecl(
                TypeSyntax.toAstType(returnT),
                returnValRange,
                name,
                args.reverse,
                body.reverse
        ))))

}

trait TypeSyntax
case object StringT extends TypeSyntax
case object StringIterT extends TypeSyntax
case class IntIterT(signed: Boolean = true, width: Int = 32) extends TypeSyntax
case class IntT(signed: Boolean = true, width: Int = 32) extends TypeSyntax
case object ErrorT extends TypeSyntax

object TypeSyntax {
    def toAstType(t: TypeSyntax): Type = t match {
        case StringT => StringType
        case StringIterT => StrIterType
        case IntIterT(signed, width) => IntIterType
        case IntT(isSigned, width) => IntDeclType(isSigned, width)
        case ErrorT => ErrorType
    }
}

object StringSyntax {
    implicit class BinOpSyntax[T <: Expr](src: T) {
        def split(delim: String): Expr = StrSplit(src, StrLit(delim, None))
        def ston: Expr = Ston(src)
    }
}
}

object SyntaxTest {
    import syntax._
    import syntax.StringSyntax._

    def main(args: Array[String]): Unit = {
        val scanner = 
            Function("scan_line")
            .returnType(IntIterT())
            .arg(Argument("clause_line").withType(StringT))
            .startFunction()
                .loop(
                    ForEach("lit")
                        .inExpr(Var("clause_line").split(" "))
                        .withAcc(Variable("literals").withType(IntIterT()))
                    .startFor()
                        .setAcc(Var("lit").ston)
                    .endFor)
                .ret(Var("literals"))
            .endFunction
        
        println(scanner)
    }
}