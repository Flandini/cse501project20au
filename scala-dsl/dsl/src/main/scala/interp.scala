/*
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

    def interpStmt(stmt: Statement, env: Environment): Environment = {
    }

    def interpExpr(exp: Expr, env: Environment): M[Expr] = 
        exp match {
            case nume: NumExpr => 
            case stre: StrExpr =>
            case itere: IterExpr =>
            case term: Term => 
            case FunCall(params) => 
        }

    def interpNumExpr(nume: Expr, env: Environment): M[Expr] = {

    }

    def interpStrExpr(stre: Expr, env: Environment): M[Expr] = {

    }

    def lookup(name: Name, env: Environment): M[Expr] =
        env match {
            case Nil => unit(Error(s"Couldn't find var: ${name}"))
            case (n, v) :: xs => 
                if (n == name)
                    unit(v)
                else
                    lookup(name, xs)
        }
}*/