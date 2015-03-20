import scala.util.{Try, Success, Failure}
import scala.collection.immutable.SortedMap

trait Concordance{
  def concordanceForString(str: String): SortedMap[String, (Int, Array[Int])] = {
    var concordance: SortedMap[String, (Int, Array[Int])] = SortedMap()
    val sentences = str.split("[.!?]\\s+(?=[A-Z]|$)")
  }
}

object Main{
  def main(args: Array[String]) = {
    val text = Try{
      args match {
        case Array(key, filename, _*) =>
          if(key == "-f") io.Source.fromFile(filename).mkString
          else throw new IllegalArgumentException("Not supported argument:  " ++ key)
        case Array(key, _*) =>
          throw new IllegalArgumentException("No value for argument: " ++ key)
        case _ =>
          io.Source.stdin.getLines.takeWhile(_.nonEmpty).mkString(" ") + " "
      }
    }
    val concordance = text.map(concordanceForString)
    concordance match{
      case Failure(e) => println(e)
      case Success(c) => c.foreach{entry => println(entry._1 + " {" + entry._2._1 +": "+ entry._2._2.mkString(", ") + "}")}
    }
  }
}
