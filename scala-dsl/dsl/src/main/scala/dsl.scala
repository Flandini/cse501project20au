
object AST {
    sealed trait Type extends Property
    case object StringType extends Type
    case object IntType extends Type 
    case object IntIterType extends Type
    case object StrIterType extends Type

    sealed trait Expr extends ForBody
    sealed trait StrExpr extends Expr
    sealed trait NumExpr extends Expr
    sealed trait IterExpr extends Expr
    // Numeric ops
    case class Add(left: Expr, right: Expr) extends NumExpr
    case class Minus(left: Expr, right: Expr) extends NumExpr
    case class Mult(left: Expr, right: Expr) extends NumExpr
    case class Div(left: Expr, right: Expr) extends NumExpr
    case class Eq(left: Expr, right: Expr) extends NumExpr
    case class Neq(left: Expr, right: Expr) extends NumExpr
    case class Gte(left: Expr, right: Expr) extends NumExpr
    case class Lte(left: Expr, right: Expr) extends NumExpr
    case class Gt(left: Expr, right: Expr) extends NumExpr
    case class Lt(left: Expr, right: Expr) extends NumExpr
    case class Or(left: Expr, right: Expr) extends NumExpr
    case class And(left: Expr, right: Expr) extends NumExpr
    case class Not(left: Expr) extends NumExpr

    case class FunCall(params: List[Expr]) extends Expr

    // String ops
    case class Ston(left: Expr) extends StrExpr
    case class StrSplit(left: Expr, right: Expr) extends StrExpr
    case class StrEquals(left: Expr, right: Expr) extends StrExpr
    case class StrConcat(left: Expr, right: Expr) extends StrExpr
    case class StrAppend(left: Expr, right: Expr) extends StrExpr
    case class StrLength(left: Expr) extends StrExpr

    // iter ops
    case class IterConcat(left: Expr, right: Expr) extends IterExpr
    case class IterFirst(left: Expr) extends IterExpr
    case class IterLength(left: Expr) extends IterExpr

    sealed trait Term extends Expr
    case class Var(name: String) extends Term
    case class IntLit(num: Int, width: Int, signed: Boolean) extends Term
    case class StrLit(value: String, range: Option[Range]) extends Term
    case class IntIter(value: List[IntLit], range: Option[Range]) extends Term
    case class StrIter(value: List[StrLit], range: Option[Range]) extends Term

    sealed trait Statement extends ForBody
    case class Decl(t: Type, name: String, expr: Option[Expr]) extends Statement
    case class If(cond: Expr, thn: Statement, els: Statement) extends Statement 
    case class For(iter1: Iterator, iter2: Option[Iterator], acc: Decl, body: List[ForBody]) extends Statement
    case class Return(expr: Expr) extends Statement

    sealed trait ForBody
    sealed trait Property
    case class FuncDecl(t: Type, range: Option[Range], name: String, args: List[Arg], body: List[Statement])
    case class Iterator(idx: String, iterator: Expr)
    case class Range(low: Int, high: Int) extends Property
    case class Arg(t: Type,
                   subrange: Option[Range],
                   range: Option[Range],
                   name: String)
    case class Error(msg: String)

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