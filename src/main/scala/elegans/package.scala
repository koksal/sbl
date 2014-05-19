package object elegans {
  object TODOException extends Exception

  private val escapeStr = (27 toChar).toString
  private val resetStr = escapeStr + "[0m"

  private val red     = "31"
  private val green   = "32"
  private val yellow  = "33"

  val runTimeStamp = timeStamp()

  private def colorize(message: String, color: String): String = {
    escapeStr + "[" + color + "m" + message + resetStr
  }

  def log(a: Any): Unit = {
    if (Settings.verbose)
      println(a.toString)
  }

  def logWarning(a: Any): Unit = {
    val warningString = colorize("[WARNING]", yellow) + " " + a.toString
    println(warningString)
  }

  def logError(a: Any): Unit = {
    val errorString = colorize("[ERROR]", red) + " " + a.toString
    println(errorString)
  }

  def logSuccess(a: Any): Unit = {
    val successString = colorize("[SUCCESS]", green) + " " + a.toString
    println(successString)
  }

  def terminate(msg: String): Nothing = {
    logError(msg)
    sys.exit(1)
  }

  def writeToFile(fname: String, content: String) {
    import java.io._
    import scala.io._
    val out = new PrintWriter(fname)
    try{ out.print( content ) }
    finally{ out.close }
  }

  def readFromFile(fname: String): String = {
    scala.io.Source.fromFile(fname).mkString
  }

  def indent(s: String): String = {
    s.split("\n").mkString("  ", "\n  ", "")
  }

  def timeStamp(): String = {
    new java.text.SimpleDateFormat("yyyy-MM-dd-kk:mm:ss").format(
      java.util.Calendar.getInstance.getTime())
  }

  import scala.sys.process._

  def testAndMakeFolder(fname: String): Unit = {
    ("mkdir -p " + fname).!!
  }

  def makeLink(fname: String, lname: String): Unit = {
    ("ln -sf " + fname + " " + lname).!!
    log("Link " + lname + " created.")
  }
}
