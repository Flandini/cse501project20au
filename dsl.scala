
object AST {
    sealed trait Type
    case object StringType extends Type
    case object IntType extends Type 
    case object IntIterType extends Type
    case object StrIterType extends Type

    sealed trait Expr extends ForBody
    // Numeric ops
    case class Add(left: Expr, right: Expr) extends Expr
    case class Minus(left: Expr, right: Expr) extends Expr
    case class Mult(left: Expr, right: Expr) extends Expr
    case class Div(left: Expr, right: Expr) extends Expr
    case class Eq(left: Expr, right: Expr) extends Expr
    case class Neq(left: Expr, right: Expr) extends Expr
    case class Gte(left: Expr, right: Expr) extends Expr
    case class Lte(left: Expr, right: Expr) extends Expr
    case class Gt(left: Expr, right: Expr) extends Expr
    case class Lt(left: Expr, right: Expr) extends Expr
    case class Or(left: Expr, right: Expr) extends Expr
    case class And(left: Expr, right: Expr) extends Expr
    case class Not(left: Expr) extends Expr

    // String ops
    case class Ston(left: Expr) extends Expr
    case class StrSplit(left: Expr, right: Expr) extends Expr
    case class StrEquals(left: Expr, right: Expr) extends Expr
    case class StrConcat(left: Expr, right: Expr) extends Expr
    case class StrAppend(left: Expr, right: Expr) extends Expr
    case class StrLength(left: Expr) extends Expr

    // iter ops
    case class IterConcat(left: Expr, right: Expr) extends Expr
    case class IterFirst(left: Expr) extends Expr
    case class IterLength(left: Expr) extends Expr

    // Smallest exprs, Term
    case class Var(name: String) extends Expr
    case class IntLit(num: Int, width: Int, signed: Boolean) extends Expr
    case class StrLit(value: String, range: Option[Range]) extends Expr
    case class IntIter(value: List[IntLit], range: Option[Range]) extends Expr
    case class StrIter(value: List[StrLit], range: Option[Range]) extends Expr

    sealed trait Statement extends ForBody
    case class Decl(t: Type, name: String, expr: Option[Expr]) extends Statement
    case class If(cond: Expr, thn: Statement, els: Statement) extends Statement 
    case class For(iter1: Iterator, iter2: Option[Iterator], acc: Decl, body: List[ForBody]) extends Statement
    case class Return(expr: Expr) extends Statement

    sealed trait ForBody
    case class FuncDecl(t: Type, range: Option[Range], name: String, args: List[Arg], body: List[Statement])
    case class Iterator(idx: String, iterator: Expr)
    case class Range(low: Int, high: Int)
    case class Arg(t: Type,
                   subrange: Option[Range],
                   range: Option[Range],
                   name: String)


    val dimacs_scanner: FuncDecl =
        FuncDecl(
        IntIterType, None, "scan_line", List(Arg(StringType, None, None, "clause_line")),
            List(
                For(
                    Iterator("lit",  StrSplit(Var("clause_line"), StrLit(" ", None))), None, Decl(IntIterType, "literals", None),
                    List(
                        Ston(Var("lit"))
                    )
                ),

                Return(Var("literals"))
            )
        )
} // End object AST