package cel.ast

// Operators start

sealed trait Operator
sealed trait BinOp
sealed trait UnOp

// Binops

case object Add extends Operator with BinOp
case object Sub extends Operator with BinOp
case object Mult extends Operator with BinOp
case object Div extends Operator with BinOp

case object Lt extends Operator with BinOp
case object Gt extends Operator with BinOp
case object Lte extends Operator with BinOp
case object Gte extends Operator with BinOp

case object NotEqual extends Operator with BinOp
case object Equal extends Operator with BinOp
case object And extends Operator with BinOp
case object Or extends Operator with BinOp

case object Append extends Operator with BinOp
case object Concat extends Operator with BinOp
case object Contains extends Operator with BinOp
case object StrEquals extends Operator with BinOp

// Unops

case object Not extends Operator with UnOp
case object Uminus extends Operator with UnOp

case object Length extends Operator with UnOp
case object Empty extends Operator with UnOp

// Operators End

// Statements Start
