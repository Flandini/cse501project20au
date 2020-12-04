import AST._ 
import PropertyStore._ 
import scala.collection.mutable.HashMap

case class SymbolTable() {
    var table = new HashMap[String, PropertyStore]()

    def nameDefined(name: String): Boolean = table.get(name) match {
        case Some(propertyStore) => true
        case None => false
    }

    def defineName(name: String): Unit = table.put(name, PropertyStore(name))
    def setPropertiesForName(name: String, p: PropertyStore): Unit = table.put(name, p)

    def getPropertyForName(name: String, propertyName: String): Option[Property] =
        for {
            properties  <- table.get(name)
            t <- properties.getAttribute(propertyName)
        } yield t

    def getTypeForName(name: String): Type = getPropertyForName(name, "type") match {
            case Some(t) => t match {
                case t: Type => t 
                case _ => ErrorType
            }
            case None => ErrorType
        }

    override def toString: String = table.toString
}