
import AST._ 
import scala.collection.mutable.HashMap
import scala.collection.immutable.Vector
import scala.collection.mutable.Stack

import cats.data.Writer
import cats.syntax.writer._
//import cats.instances.vector._
import cats.syntax.applicative._ // for pure
import scala.sys.Prop

object TypeChecker {
    // Each variable can have propreties like ranges, type
    // varname/funcname/etc => (propertyname => property)
    type SymbolTable = HashMap[String, HashMap[String, Property]]
    type Errors = Vector[String]
    type Result = (Type, Errors)
    //type Result[A] = Writer[Vector[String], A]

    var table: SymbolTable = new HashMap()
    var errors: Errors = Vector()
    var funcStack: Stack[String] = Stack()

    def checkFunc(func: FuncDecl): Result = 
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
                (typ, errors)
            }
        }

    def checkStmt(stmt: Statement): Result =
        stmt match {
            case Return(expr) => {
                val (typ, preverrors) = checkExpr(expr)
                val currentFunc = funcStack.top
                val expected = 
                    table.get(currentFunc) match {
                        case Some(traits) => traits.get("type")
                        case None => None
                    }
                expected match {
                    case Some(otherType) => {
                        if (otherType == typ)
                            (typ, errors)
                        else {
                            errors ++ s"Return type of ${expr} doesnt match expected ${otherType}"
                            (typ, errors)
                        }
                    }
                    case None => {
                        errors ++ s"Return type of ${expr} wrong"
                        (typ, errors)
                    }
                }
            }
            case For(iter1, iter2, acc, body) => (StringType, errors)
            case If(cond, thn, els) => (StringType, errors)
            case Decl(t, name, expr) => (StringType, errors)
        }


    def checkIterator(iter: Iterator): Result = 
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

    def checkArg(arg: Arg): Result = {
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
                (typ, errors)
            }
            case None => {
                var traits = new HashMap[String, Property]()
                traits.put("type", typ)
                range match {
                    case None => {}
                    case Some(r) => traits.put("range", r)
                }
                subrange match {
                    case None => {}
                    case Some(r) => traits.put("subrange", r)
                }
                table.put(name, traits)
            }
        }
        
        (typ, errors)
    }

    def checkExpr(expr: Expr): Result = {
        expr match {
            case Add(l, r) => (StringType, errors)
            case Minus(l,r) => (StringType, errors)
            case Mult(l,r) => (StringType, errors)
            case Div(l,r) => (StringType, errors)
            case Eq(l,r) => (StringType, errors)
            case Neq(l,r) => (StringType, errors)
            case Gte(l,r) => (StringType, errors)
            case Lte(l,r) => (StringType, errors)
            case Gt(l,r) => (StringType, errors)
            case Lt(l, r) => (StringType, errors)
            case Or(l,r) => (StringType, errors)
            case And(l, r) => (StringType, errors)
            case Not(l) => (StringType, errors)

            case Ston(l) => (StringType, errors)
            case StrSplit(l,r) => (StringType, errors)
            case StrEquals(l, r) => (StringType, errors)
            case StrConcat(l, r) => (StringType, errors)
            case StrAppend(l, r) => (StringType, errors)
            case StrLength(l) => (StringType, errors)

            case IterConcat(l,r) => (StringType, errors)
            case IterFirst(l) => (StringType, errors)
            case IterLength(l) => (StringType, errors)

            case FunCall(name, params) => (StringType, errors)
            case Var(name) => (StringType, errors)
            case IntLit(num, width, signed, range) => (StringType, errors)
            case StrLit(value, range) => (StringType, errors)
            case IntIter(value, range) => (StringType, errors)
            case StrIter(value, range) => (StringType, errors)
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