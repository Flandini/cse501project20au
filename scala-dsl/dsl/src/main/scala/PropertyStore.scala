import AST._
import scala.collection.mutable.HashMap

case class PropertyStore(name: String) {
    var table = new HashMap[String, Property]()

    override def toString: String = s"${name} :: ${table}"

    def setRange(r: Option[Range]): PropertyStore = {
        for {
            range <- r
        } yield table.put("range", range)
        this 
    }

    def setSubrange(sr: Option[Range]): PropertyStore = {
        for {
            subrange <- sr
        } yield table.put("subrange", subrange)
        this 
    }

    def setType(t: Type): PropertyStore = {
        table.put("type", t)
        this 
    }

    def setArity(a: Int): PropertyStore = { 
        table.put("arity", Arity(a))
        this
    }

    def hasType(t: Type): Boolean = table.get("type") match {
        case Some(actualType) => t == actualType
        case None => false
    }

    def getAttribute(attr: String): Option[Property] = table.get(attr)
}