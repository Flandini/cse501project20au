
import AST._ 

object idenM {
    type M[A] = A
    def unit[A](a: A): M[A] = a
    def flatMap[A, B](a: M[A], b: A => M[B]): M[B] = b(a)
}

object Interpreter {

    type Name = String
    type Environment = List[(Name, Expr)]

    import idenM._

    def interp(program: List[FuncDecl], env: Environment): M[Expr] = {
    }

    def interpFunc(func: FuncDecl, env: Environment): M[Expr] = {
    }

    def interpStmt(stmt: Statement, env: Environment): M[Expr] = {
    }
}