

object AST {
    // 48
    abstract class Type extends Property
    case object StringType extends Type
    case object IntType extends Type 
    case object IntIterType extends Type
    case object StrIterType extends Type
    case object ProgramType extends Type
    case object ErrorType extends Type
    case class IntDeclType(signed: Boolean, width: Int) extends Type

    def signed = true
    def unsigned = false

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
    case class Ntos(left: Expr) extends Expr

    case class FunCall(name: String, params: List[Expr]) extends Expr

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

    sealed trait Term extends Expr
    case class Var(name: String) extends Term
    case class IntLit(num: Int, width: Int, signed: Boolean, range: Option[Range]) extends Term
    case class StrLit(value: String, range: Option[Range]) extends Term
    case class IntIter(value: List[IntLit], range: Option[Range], subrange: Option[Range]) extends Term
    case class StrIter(value: List[StrLit], range: Option[Range], subrange: Option[Range]) extends Term

    sealed trait Statement extends ForBody
    case class Decl(t: Type, name: String, expr: Option[Expr]) extends Statement
    case class If(cond: Expr, thn: List[Statement], els: Option[List[Statement]]) extends Statement 
    case class For(iter1: Iterator, iter2: Option[Iterator], acc: Decl, body: List[ForBody]) extends Statement
    case class Return(expr: Expr) extends Statement

    sealed trait ForBody
    sealed trait Property
    case class FuncDecl(t: Type, range: Option[Range], name: String, args: List[Arg], body: List[Statement])
    case class Iterator(idx: String, iterator: Expr)
    case class Range(low: Int, high: Int) extends Property
    case class Arity(num: Int) extends Property
    case class Arg(t: Type,
                   subrange: Option[Range],
                   range: Option[Range],
                   name: String)
    case class Program(funcs: List[FuncDecl])
} // End object AST

import AST._

object DslSyntax {
    
}

object ExamplePrograms {
    val dimacs_scanner: Program =
        Program(List(
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
        )))

    val array_average =
        Program(List(
        FuncDecl(
        IntType, Some(Range(0, 255)), "array_average", List(Arg(IntIterType, Some(Range(0, 255)), Some(Range(0, 50000)), "numbers")), List(
        For(Iterator("n", Var("numbers")), None, Decl(IntDeclType(unsigned, 32), "acc", Some(IntLit(0, 32, signed, None))), List(
            Add(Var("acc"), Var("n"))
        )),
        Decl(IntDeclType(unsigned, 8), "averagish", Some(Div(Var("acc"), IterLength(Var("numbers"))))),
        Return(Var("averagish"))
        ))))

    val one_decl_one_return =
    Program(List(
        FuncDecl(
        IntType, Some(Range(0, 255)), "array_average", List(Arg(IntType, None, Some(Range(0, 255)), "input")), List(
        Decl(IntDeclType(signed, 32), "averageish", Some(Var("input"))),
        Return(Var("averageish"))
        ))))
}