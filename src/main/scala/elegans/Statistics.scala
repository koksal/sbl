package elegans

object Statistics {
  var solverCallTimes = List[Long]()
  var beginning: Long = 0L
  var end: Long = 0L
  var totalRunProfiler = new Stopwatch("total-run")
  var solverRunning = false

  def runStarted() {
    totalRunProfiler.start
  }

  def runEnded() {
    totalRunProfiler.stop
  }

  def solverCalled() {
    assert(!solverRunning)
    solverRunning = true
    beginning = System.currentTimeMillis
  }

  def solverReturned() {
    assert(solverRunning)
    solverRunning = false
    end = System.currentTimeMillis
    solverCallTimes = (end - beginning) :: solverCallTimes
  }

  def printSummary() {
    val columns = List("time", "# calls", "avg. call time")
    val nbCalls = solverCallTimes.size
    val accumulatedCallTime = solverCallTimes.foldLeft(0L)(_ + _)
    val avgCallTime = if (nbCalls == 0L) 0L else (accumulatedCallTime / nbCalls)
    val avgCallTimeSec = avgCallTime / 1000.0
    val totalTime = totalRunProfiler.acc / 1000.0
    val values = List[String](totalTime.toString, nbCalls.toString, avgCallTimeSec.toString)

    println("SUMMARY: " + columns.mkString(" & "))
    println("SUMMARY: " + values.mkString(" & "))
  }

}
