package elegans

/** Implements a stopwatch for profiling purposes */
class Stopwatch(description : String, verbose : Boolean = false) {
  var beginning: Long = 0L
  var end: Long = 0L
  var acc: Long = 0L

  def start : Stopwatch = {
    beginning = System.currentTimeMillis
    this
  }

  def stop : Double = {
    end = System.currentTimeMillis
    acc += (end - beginning)
    val seconds = (end - beginning) / 1000.0
    if (verbose) println("Checkpoint %-25s: %-3.2fs" format (description, seconds))
    seconds
  }

  def summarize: Unit = {
    val seconds = acc / 1000.0
    log("Total for %-25s: %-3.2fs" format (description, seconds))
  }
}
