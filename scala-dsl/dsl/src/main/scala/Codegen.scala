
import AST._
import SafetyChecks._
import Interval._
import SymbolTable._
import PropertyStore._

import scala.collection.mutable.HashMap
import scala.io.Source

object CodeGen {
    type Code = String
    type Range = (Option[IntervalLatticeElement], Option[IntervalLatticeElement])

    // Data carried over from SafetyChecks (interval data)
    var constraintData: SafetyChecks.Env = List[VarInfo]()

    // Declarations appearing at the top of the function body
    var declarations: List[Code] = List()

    // frees appearing at the bottom of the funciton body
    var frees: List[Code] = List()

    // Map of expressions to variable names
    // e.g. in for (x : str_split(str, " ")), the str_split expr
    // will be set to a declaration and assignment to a fresh var 
    // at top of function body, so then exprToNames[str_split(str, " ")] 
    // returns that fresh var name
    var exprToNames: HashMap[Expr, Code] = new HashMap()

    // Type information from type checking
    var types: SymbolTable = new SymbolTable()

    // For generating fresh variable names
    var iterNameIdx = 0;
    var idxNameIdx = 0;
    var delimIdx = 0;
    var dupIdx = 0;
    var passThrough = 0;

    // The 'runtime' or library of str and iter functions
    // TODO: Unhardcode. Just doing this while i'm doing testing in sbt
    val libfile = "/Users/mkf727/projects/dsl/scala-dsl/dsl/src/main/scala/lib.h"

    def lookup(v: VarName, env: Env): Range = env match {
        case Nil => (None, None)
        case (varname, t, latticeE, olatticeE, value) :: xs => if (v == varname) 
                                                                    (latticeE, olatticeE)
                                                                else
                                                                    lookup(v, xs)
    }

    def addDecl(decl: Code): Unit = declarations = declarations ++ List(decl)
    def addFree(free: Code): Unit = frees = frees ++ List(free)
    def addFreeOfVarName(varname: Code): Unit = {
        val freeStmt = s"free (${varname});"
        addFree(freeStmt)
    }

    def getNextIterName: Code = {
        val res = s"iterator${iterNameIdx}"
        iterNameIdx = iterNameIdx + 1 
        res
    }
    def getNextPassThroughName: Code = {
        val origVal = passThrough
        passThrough = passThrough + 1
        s"pass_through_${origVal}"
    }
    def getNextIdxName: Code = {
        val origVal = idxNameIdx
        idxNameIdx = idxNameIdx + 1
        s"i_${origVal}"
    }

    def genPreconditions: Code = """"""

    def tab: Code = " " * 4
    def addIndent(indentLevel: Int): Code = tab * indentLevel

    def genStmt(stmt: Statement, indent: Int = 1): Code = {
        val prependStr = " " * 4 * indent
        val stmtStr = stmt match {
            case Decl(t, name, expr) => expr match {
                case None => s" ${genType(t)} ${name} "
                case Some(e) =>  s"${genType(t)} ${name} = ${genExpr(e)};\n"
            }
            case For(iter1, iter2, acc, body) => genForStmt(For(iter1, iter2, acc, body), indent)
            case If(cond, thn, els) => genIfStmt(If(cond, thn, els), indent)
            case Return(expr) => { 
                val freestmts = frees.map((" " * 4) + _).mkString("\n") + "\n"
                val ret = s"return ${genExpr(expr)};\n"
                freestmts ++ ret
            }
        }
        prependStr ++ stmtStr
    }

    // Returns tuple of idx var name and iter var name
    def genIterator(iter: Iterator): (Code, Code) = {
        val idxName = iter.idx
        val iterExpr = iter.iterator

        val idxType = types.getTypeForName(idxName)

        addDecl(s"${genType(idxType)} ${idxName};")

        val iterCode = genExpr(iterExpr)

        (idxName, iterCode)
    }

    def genForBody(f: ForBody, indent: Int): Code = f match {
        case e : Expr => genExpr(e)
        case s : Statement => genStmt(s, indent)
        case _ => "ERROR IN CODE GEN"
    }

    // currently only handling one iterator and for bodies with just one expr in it (the acc value)
    def calloc(size: Code, num: Code): Code = s"calloc(${size}, ${num});"
    def genForStmt(f: For, indent: Int = 1): Code = {
        val (idx, iter) = genIterator(f.iter1)
        val i = getNextIdxName
        val header = 
            s"for (uint32_t ${i} = 0; (${idx} = ${iter}[${i}]); ++${i}) {\n"
        val closer = "}"

        val accName = f.acc.name
        val accType = f.acc.t
        val accStmt = genStmt(f.acc, indent)
        
        val accInitVal = f.acc.expr match {
            case None => 
                if (f.acc.t == IntIterType) {
                    s" = calloc(int_iter_length(${iter}), sizeof(int32_t));"
                }
                else if (f.acc.t == StrIterType) {
                    s" = calloc(char_iter_length(${iter}), sizeof(char*));"
                } else {
                    ";"
                }

            case Some(e) => ""
        }
        val accDecl = accStmt + accInitVal 

        val sink = if (f.acc.t == IntIterType || f.acc.t == StrIterType)
                        s"${accName}[${i}]"
                   else
                        accName

        val sinkStmt = s"${sink} = ${genForBody(f.body(0), indent)};"
        
        var output = accDecl ++ "\n"
        output = output ++ addIndent(indent) ++ header ++ "\n"
        output = output ++ addIndent(indent + 1) ++ sinkStmt ++ "\n"
        output = output ++ addIndent(indent) ++ closer ++ "\n\n"

        output
    }

    def genIfStmt(ifStmt: If, indent: Int = 1): Code = {
        val prependStr = " " * 4 * indent
        val check = s"if ( ${genExpr(ifStmt.cond)} ) {\n"
        val thenBody = ifStmt.thn.map(genStmt(_, indent + 1)).mkString
        val thenClose = "}"
        val elsePart: Code = ifStmt.els match {
            case None => ""
            case Some(stmts) => {
                val elsePart = s" else {\n"
                val elseBody = stmts.map(genStmt(_, indent + 1)).mkString
                val elseClose = s"}"
                elsePart ++ elseBody ++ prependStr ++ elseClose
            }
        }
        prependStr ++ check ++ thenBody ++ prependStr ++ thenClose ++ elsePart
    }

    def genType(t: Type): Code = t match {
        case StringType => "char*"
        case IntType => "int32_t" // TODO: ??
        case StrIterType => "char**"
        case IntIterType => "int32_t*"
        case IntDeclType(signed: Boolean, width: Int) => {
            val sign = if (signed) "" else "u"
            s"${sign}int${width}_t"
        }
        case ProgramType => "ERROR IN CODEGEN" // Shouldn't propagate to here
        case ErrorType => "ERROR IN CODEGEN" // Shouldn't propagate to here
    }

    def genProg(p: Program): Code = {
        val rangeData = SafetyChecks.check(p)
        rangeData match {
            case Left(err) => {} // Should work by here or safety and type checks would've failed
            case Right(constraintTable) => constraintData = constraintTable
        }

        types = TypeChecker.getTypes(p)

        val header = genLibrary
        val funcs = genFunc(p.funcs(0))
        header ++ funcs
    }

    def genFunc(f: FuncDecl): Code = f match {
        case FuncDecl(t, range, name, args, body) => {
            val typeStr = genType(t)
            val argslist = args.map(genArg).mkString(", ")
            val prologue = s"${typeStr} ${name}(${argslist}) {"

            val statementsStr = body.map(genStmt(_)).mkString

            val decls = declarations.map((" " * 4) + _).mkString("\n") + "\n"
            val epilogue = "}"

            prologue ++ "\n" ++ decls ++ statementsStr ++ epilogue ++ "\n\n"
        }
    }

    def genArg(a: Arg): Code = genType(a.t) ++ " " ++ a.name

    def genExpr(expr: Expr): Code = {
        expr match {
            case Add(l, r) => s"${genExpr(l)} + ${genExpr(r)}"
            case Minus(l,r) => s"${genExpr(l)} - ${genExpr(r)}"
            case Mult(l,r) => s"${genExpr(l)} * ${genExpr(r)}"
            case Div(l,r) => s"${genExpr(l)} / ${genExpr(r)}"
            case Eq(l,r) => s"${genExpr(l)} == ${genExpr(r)}"
            case Neq(l,r) => s"${genExpr(l)} != ${genExpr(r)}"
            case Gte(l,r) => s"${genExpr(l)} >= ${genExpr(r)}"
            case Lte(l,r) => s"${genExpr(l)} <= ${genExpr(r)}"
            case Gt(l,r) => s"${genExpr(l)} > ${genExpr(r)}"
            case Lt(l, r) => s"${genExpr(l)} < ${genExpr(r)}"
            case Or(l,r) => s"${genExpr(l)} || ${genExpr(r)}"
            case And(l, r) => s"${genExpr(l)} && ${genExpr(r)}"
            case Not(l) => s"!(${genExpr(l)})"
            case Ntos(l) => s"itoa(${genExpr(l)})"

            case Ston(l) => s"atoi(${genExpr(l)})"
            case StrSplit(l,r) => genStrSplit(StrSplit(l, r))
            case StrEquals(l, r) => "" // TODO: Custom gen method
            case StrConcat(l, r) => "" // TODO: Custom gen method
            case StrAppend(l, r) => "" // TODO: Custom gen method
            case StrLength(l) => s"strlen(${genExpr(l)})" // TODO: Custom gen method

            // need range/constraint info from typechecking/safetychecks
            case IterConcat(l,r) => "" // TODO: Custom gen method
            case IterFirst(l) => s"${genExpr(l)}[0]"
            case IterLength(l) => s"iter_length((void**) ${genExpr(l)})"

            case FunCall(name, params) => name + "(" + params.map(genExpr).mkString(",") + ")"
            case Var(name) => name

            case IntLit(num, width, signed, range) => num.toString
            case StrLit(value, range) => "\"" + value + "\""
            case IntIter(value, range, subrange) => "ERROR IN CODEGEN" 
            case StrIter(value, range, subrange) => "ERROR IN CODEGEN"
        }
    }

    def dupName(name: Code): Code = {
        val origVal = dupIdx
        dupIdx = dupIdx + 1
        s"${name}_dup_${origVal}"
    }
    def genStrSplit(expr: StrSplit): Code = {
        // delimiter gen
        val delimName = s"delim${delimIdx}"
        delimIdx = delimIdx + 1
        exprToNames.put(expr.right, delimName)
        val delimDecl = s"char* ${delimName} = ${genExpr(expr.right)};"
        addDecl(delimDecl)

        // Dupe string for splitting in case it is not compat w/ strtok/strsep
        // pass through is necessary in case expr.left is not a var name
        val targetStr = genExpr(expr.left)
        val passthroughStrName = getNextPassThroughName
        val passThroughStmt = s"char* ${passthroughStrName} = ${targetStr};"
        addDecl(passThroughStmt)
        val dupStrName = dupName(passthroughStrName)
        val dupStmt = s"char* ${dupStrName} = strdup(${passthroughStrName});"
        addDecl(dupStmt)
        addFreeOfVarName(dupStrName)

        val newIterName = getNextIterName
        val splitStmt = s"char** ${newIterName} = str_split(${dupStrName}, ${delimName});"
        addDecl(splitStmt)
        exprToNames.put(expr, newIterName)
        newIterName
    }

    def exprIsVarName(expr: Expr): Boolean = expr match {
        case Var(name) => true 
        case _ => false
    }

    def genLibrary: Code = Source.fromFile(libfile).getLines.mkString("\n")
}

object CodegenTest {
    import CodeGen._
    import ExamplePrograms._

    def main(args: Array[String]): Unit = {
        //println(genProg(one_decl_one_return))
        println(genProg(dimacs_scanner))
        //println(genProg(array_average))
    }
}