package ndpc
import scala.io.Source
import ndpc.Parser._
import ndpc.expr.Formula._

object Formatter {
    def format(input: List[String], apply: Boolean): Int = 0

    private def maxOf(target1 : Int, target2: Int) : Int = if (target2 > target1) target2 else target1

    private def findReasonAlign(target : PfScope, ini_indent : Int = 0) : Int = {
        var result = 0
        target.body.foreach((line)=> if (line.isInstanceOf[PfScope]){
                //go deeper if we see another block
                result = maxOf(result, findReasonAlign(line.asInstanceOf[PfScope], ini_indent = ini_indent + 1))
            }else if (line.isInstanceOf[Pf]){
                // update result if we see a new useful line
                result = maxOf(result, (line.asInstanceOf[Pf]).concl.toString().length())
            })
        return result + ini_indent * 2
    }

    private def lineParser(line : Line, indent : Int, reasonAlign : Int) : String = {
        line match{
            case Empty() => return ""
            case Comment(contents) => return " " * indent * 2 + contents
            case Pf(concl,rule,None) =>return " " * indent * 2 + 
                                                     concl.toString()+ " " + " " * (reasonAlign - concl.toString().length() - indent * 2) + "[" +
                                                     rule.toString() + "]"
            case Pf(concl, rule, Some(comment)) => return " " * indent * 2 + 
                                                     concl.toString()+ " " + " " * (reasonAlign - concl.toString().length() - indent * 2) + "[" +
                                                     rule.toString() + "] -- " + comment
        }
    }

    def scopeFormatter(target : PfScope, currentIndent : Int, reasonAlign : Int) : String = {
        var result = ""
        target.body.foreach((line)=>
            if (line.isInstanceOf[PfScope]){
                result += scopeFormatter(line.asInstanceOf[PfScope], currentIndent + 1, reasonAlign)
            }else{
                result += lineParser(line.asInstanceOf[Line], currentIndent, reasonAlign) + "\n"
            })
        return result
    }
    
    def formatter(target:UncheckedProof | CheckedProof) : String = {
        val thisScope = target match{
            case UncheckedProof(main, lines) => main
            case CheckedProof(main) => main
        }
        return scopeFormatter(thisScope , 0, findReasonAlign(thisScope))
    }
}
