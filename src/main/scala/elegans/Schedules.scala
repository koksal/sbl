package elegans

object Schedules {
  sealed trait Configuration
  case object EnabledRight extends Configuration {
    override def toString: String = ">"
  }
  case object EnabledLeft extends Configuration {
    override def toString: String = "<"
  }
  case object Disabled extends Configuration {
    override def toString: String = "-"
  } 

  /* A micro-step indicates which cells move at each clock tick */
  type MicroStep = List[Boolean]
  type MacroStep = List[Configuration]
  type FineSchedule = List[MicroStep]
  type CoarseSchedule = List[MacroStep]

  def allConfigurations(nbChannels: Int): List[List[Configuration]] = {
    if (nbChannels == 1)
      List(List(EnabledRight), List(EnabledLeft), List(Disabled))
    else {
      val rest = allConfigurations(nbChannels - 1)
      rest.map(cs => List(EnabledRight :: cs, EnabledLeft :: cs, Disabled :: cs)).flatten
    }
  }

  import scala.collection.mutable.{Map => MutableMap}

  private val cellConfigCache = MutableMap[Int, List[List[Configuration]]]()

  def cellConfigurations(nbCells: Int): List[List[Configuration]] = cellConfigCache.get(nbCells) match {
    case Some(conf) => conf
    case None => {
      val newConfs = allConfigurations(nbCells - 1)
      cellConfigCache(nbCells) = newConfs
      newConfs
    }
  }

  val random = new scala.util.Random()

  /** Random schedule for six cells, with init step */
  def randomMicroStepSchedule(nbCells: Int, length: Int): List[MicroStep] = {
    val ils = randomMacroStepSchedule(nbCells, length) map interleaving
    ils.flatten
  }

  def randomMacroStepSchedule(nbCells: Int, length: Int): List[MacroStep] = {
    val confs = (0 to length).toList map { _ => 
      val confs = cellConfigurations(nbCells)
      confs(random.nextInt(confs.size))
    }
    confs
  }

  def syncCoarseSchedule(nbCells: Int, length: Int): List[MacroStep] = {
    val css = for (i <- 1 to length) yield {
      val cs = for (j <- 1 until nbCells) yield Disabled
      cs.toList
    }
    css.toList
  }

  def syncFineSchedule(nbCells: Int, length: Int): List[MicroStep] = {
    val ils = syncCoarseSchedule(nbCells, length).toList map interleaving
    ils.flatten
  }

  def fineFromCoarse(coarse: CoarseSchedule): FineSchedule = {
    (coarse map interleaving) flatten
  }

  def indicesByType(cs: List[Configuration], toFind: Configuration): List[Int] = {
    cs.zipWithIndex.filter(_._1 == toFind).unzip._2
  }

  def interleaving(configurations: List[Configuration]): List[MicroStep] = {
    val nbCells = configurations.size + 1
    import scala.collection.mutable.ListBuffer
    val sequence = ListBuffer[List[Int]]()

    val rightEnabled = indicesByType(configurations, EnabledRight)
    val leftEnabled = indicesByType(configurations, EnabledLeft)

    def handleRange(start: Int, end: Int) {
      val toHandle = start to end
      val leftEnabledToHandle = leftEnabled.filter(i => i >= toHandle.head && i <= toHandle.last)
      var lastL = end
      for (l <- leftEnabledToHandle.reverse) {
        val toMove = (l + 1) to lastL
        lastL = l
        sequence append toMove.toList
      }
      val remaining = if (leftEnabledToHandle.isEmpty) (start to end) 
        else (start to leftEnabledToHandle.head)

      sequence append remaining.toList
    }

    def microStepFromIndices(is: List[Int]): MicroStep = {
      val ms = for (i <- 0 until nbCells) yield
        if (is.indexOf(i) >= 0) true else false

      ms.toList
    }
    
    var lastR = -1
    for (r <- rightEnabled) {
      handleRange(lastR + 1, r)
      lastR = r
    }
    handleRange(lastR + 1, configurations.length)

    val noEmptySteps = sequence.toList.filter(_ != Nil)

    noEmptySteps map microStepFromIndices
  }

  def microStepStates(microsteps: List[MicroStep]): List[List[Boolean]] = {
    val nbCells = microsteps.head.size
    var currentStep = (0 until nbCells).toList.map(_ => false)
    val ret = for (m <- microsteps) yield {
      val zipped = currentStep zip m
      val newStep = zipped.map(pair => pair._1 || pair._2)
      currentStep = newStep
      newStep
    }

    ret.toList
  }

  def isValid(ms: MicroStep): Boolean = {
    var lastSeen: Option[Boolean] = None
    var alternationCount = 0
    for (b <- ms) lastSeen match {
      case Some(last) => 
        if (b != last) {
          alternationCount += 1
          lastSeen = Some(b)
        }
      case None =>
        lastSeen = Some(b)
    }

    alternationCount <= 3
  }

  def scheduleString(s: List[MicroStep]) : String = {
    s.map(ms => ms.map(b => if (b) "1" else "0").mkString("")).mkString("\n")
  }

  def printOcamlLists(ils: List[List[List[Boolean]]]) {
    println(ils.map(_.map(_.mkString("[",";",";]")).mkString("  [",";\n  ",";]")).mkString("[", ";\n\n", ";]"))
  }

  def printOcamlSchedule(ils: List[List[Boolean]]) {
    println((ils.map(_.mkString("[",";",";]")).mkString("  [",";\n  ",";]")))
  }
}
