
import AST._ 
import scala.collection.mutable.HashMap

object TypeChecker {
    // Each variable can have propreties like ranges, type
    // varname/funcname/etc => (propertyname => property)
    type SymbolTable = HashMap[String, HashMap[String, Property]]
    var table: SymbolTable = new HashMap()

    def check(program: List[FuncDecl]): Boolean = {
        true 
    }
}

object Test {
    def main(args: Array[String]): Unit = {
        println("hello")
    }
}