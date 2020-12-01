
import AST._ 
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

//import cats.data.Writer
//import cats.syntax.writer._
import cats.instances.vector._
import cats.syntax.applicative._ // for pure
import scala.sys.Prop

object TypeChecker {
    // Each variable can have propreties like ranges, type
    // varname/funcname/etc => (propertyname => property)
    type SymbolTable = HashMap[String, HashMap[String, Property]]
    type Errors = Vector[String]
    type Result = (Type, Errors)

    type Writer[A] = (Errors, A)
    def unit[A](a: A): Writer[A] = (Vector(), a)
    def error(msg: String): Writer[Unit] = (Vector(msg), ())
    def flatMap[A, B](a: Writer[A], b: A => Writer[B]): Writer[B] = {
        val (errs, value) = a
        val (newerrs, newvalue) = b(value)
        (errs ++ newerrs, newvalue)
    }

    var table: SymbolTable = new HashMap()
    var errors: Errors = Vector()
    var funcStack: Stack[String] = Stack()

    def checkFunc(func: FuncDecl): Writer[Type] = 
        func match {
            case FuncDecl(typ, range, name, args, body) => {
                // Check func not already declared
                table.get(name) match {
                    case None => table.put(name, new HashMap())
                    case Some(res) => errors ++ s"Func ${name} already defined"
                }

                table.get(name) match {
                    case None => {}
                    case Some(res) => {
                        res.put("type", typ)
                        range match {
                            case None => {}
                            case Some(r) => res.put("range", r)
                        }
                        res.put("arity", Arity(args.length))
                        funcStack.push(name)
                    }
                }

                args.foreach(checkArg)
                body.foreach(checkStmt)

                println(table)
                println(funcStack)

                funcStack.pop()
                (errors, typ)
            }
        }

    def checkStmt(stmt: Statement): Writer[Type] =
        stmt match {
            case Return(expr) => {
                val (preverrors, typ) = checkExpr(expr)
                val currentFunc = funcStack.top
                val expected = 
                    table.get(currentFunc) match {
                        case Some(traits) => traits.get("type")
                        case None => None
                    }
                expected match {
                    case Some(otherType) => {
                        if (otherType == typ)
                            (errors, typ)
                        else {
                            errors ++ s"Return type of ${expr} doesnt match expected ${otherType}"
                            (errors, typ)
                        }
                    }
                    case None => {
                        errors ++ s"Return type of ${expr} wrong"
                        (errors, typ)
                    }
                }
            }
            case For(iter1, iter2, acc, body) => (errors, StringType)
            case If(cond, thn, els) => (errors, StringType)
            case Decl(t, name, expr) => (errors, StringType)
        }


    def checkIterator(iter: Iterator): Writer[Type] = 
        iter match {
            case Iterator(idx, itere) => {
                table.get(idx) match {
                    case None => { 
                        table.put(idx, new HashMap())
                        checkExpr(itere)
                    }
                    case Some(traits) => {
                        errors ++ s"Iterator ${idx} shadows another variable and is not allowed"
                        checkExpr(itere)
                    }
                }
            }
        }

    def checkArg(arg: Arg): Writer[Type] = {
        val typ = arg.t
        val subrange = arg.subrange
        val range = arg.range
        val name = arg.name

        subrange match {
            case Some(range) =>
                if (typ == IntType || typ == StringType)
                    errors ++ s"${name} is a ${typ} which cannot have a subrange"
            case None => {}
        }

        table.get(name) match {
            case Some(traits) => {
                errors ++ s"Args cannot have the same name ${name}"
                (errors, typ)
            }
            case None => {
                var traits = new HashMap[String, Property]()
                traits.put("type", typ)

                for { 
                    r <- range
                    sr <- subrange
                } yield { traits.put("range", r); traits.put("subrange", r) }

                table.put(name, traits)
            }
        }
        
        (errors, typ)
    }

    def checkExpr(expr: Expr): Writer[Type] = {
        expr match {
            case Add(l, r) => (errors, StringType)
            case Minus(l,r) => (errors, StringType)
            case Mult(l,r) => (errors, StringType)
            case Div(l,r) => (errors, StringType)
            case Eq(l,r) => (errors, StringType)
            case Neq(l,r) => (errors, StringType)
            case Gte(l,r) => (errors, StringType)
            case Lte(l,r) => (errors, StringType)
            case Gt(l,r) => (errors, StringType)
            case Lt(l, r) => (errors, StringType)
            case Or(l,r) => (errors, StringType)
            case And(l, r) => (errors, StringType)
            case Not(l) => (errors, StringType)

            case Ston(l) => (errors, StringType)
            case StrSplit(l,r) => (errors, StringType)
            case StrEquals(l, r) => (errors, StringType)
            case StrConcat(l, r) => (errors, StringType)
            case StrAppend(l, r) => (errors, StringType)
            case StrLength(l) => (errors, StringType)

            case IterConcat(l,r) => (errors, StringType)
            case IterFirst(l) => (errors, StringType)
            case IterLength(l) => (errors, StringType)

            case FunCall(name, params) => (errors, StringType)
            case Var(name) => (errors, StringType)
            case IntLit(num, width, signed, range) => (errors, StringType)
            case StrLit(value, range) => (errors, StringType)
            case IntIter(value, range) => (errors, StringType)
            case StrIter(value, range) => (errors, StringType)
        }
    }

}

object Test {
    import TypeChecker._

    def main(args: Array[String]): Unit = {
        val res = checkFunc(dimacs_scanner)
        println(res)
    }
}