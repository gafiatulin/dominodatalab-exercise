
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
  }
}
