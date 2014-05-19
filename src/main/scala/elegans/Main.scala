package elegans

object Main {
  import Model._
  import Experiments._
  import Interpreter._
  import Constraints._
  import Schedules._
  import RunningMethods._

  import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

  var noErrors = true

  def main(args: Array[String]): Unit = {
    Statistics.runStarted()
    // process command-line options
    val (options, _) = args.toList.partition(_.startsWith("--"))
    processOptions(options map (_.substring(2)))

    val allExperiments = Experiments.parseExperiments("specifications/current-specification")

    val toCheck = restrictExperimentRange(allExperiments)

    Settings.toPerform match {
      case Run => runSimply(toCheck)
      case RandomTest(nbTests) => runRandomTests(nbTests, toCheck)
      case Verify => runVerifier(toCheck)
      case Solve => runSolver(toCheck)
      // todo why take head here? fix it.
      case SolveForDifferentOutcome(toDiff) => runDiffSolver(toDiff.head, toCheck)
      case PruneExperiments(toPrune) => runPruning(toPrune)
      case CompareOutcomeSets(outcomeSets) => runDifferentiation(outcomeSets)
      case SimplifyAutomata => runSimplification(toCheck)
      case SummarizeTraces => runSummarization(toCheck)
      case CollectFates => runFateCollection(toCheck)
    }

    if (noErrors) 
      logSuccess("Completed without errors.")
    else
      logError("There were errors.")

    Statistics.runEnded()
    Statistics.printSummary()
    // translationProfiler.summarize
    // synthesisProfiler.summarize
    // verificationProfiler.summarize
    // runningProfiler.summarize
  }

  private def restrictExperimentRange(allExperiments: List[Experiment]) = Settings.mutations match {
    case Some((i1, i2)) => 
      assert(i1 >= 0 && i1 < allExperiments.length &&
             i2 >= i1 && i2 < allExperiments.length, "Bad mutation index")
      allExperiments.slice(i1, i2 + 1)
    case None => allExperiments
  }

  private def runRandomTests(nbTests: Int, toCheck: Seq[Experiment]) = {
    val seenOutcomes = MutableMap[Experiment, Set[Seq[String]]]()

    for (i <- 1 to nbTests) {
      val randomSched = 
        randomMacroStepSchedule(Model.nbAsyncCells, Settings.runLength)
      for (exp <- toCheck) {
        Settings.runningMethod(exp, randomSched, Settings.solution) match {
          case Some((cells, trace)) => {
            // add outcome to set of seen outcomes
            val decided = decidedFates(cells, trace)
            seenOutcomes.get(exp) match {
              case Some(set) => seenOutcomes(exp) = set + decided
              case None =>      seenOutcomes(exp) = Set(decided)
            }
          }
          case None => noErrors = false
        }
      }

      summarizeSeenOutcomes(seenOutcomes)
    }
  }

  private def runSolver(toCheck: List[Experiment]) {
    val solutions = CEGIS.solve(toCheck, Settings.nbMaxModels)
    noErrors &&= solutions.size == Settings.nbMaxModels
    Settings.solution match {
      case Some(_) => logWarning("Provided solution is not used.")
      case None =>
    }
  }

  private def runVerifier(toCheck: List[Experiment]) {
    // Instantiate a dummy solution if no file was specified
    val sol = Settings.solution.getOrElse(Map())
    noErrors &&= Constraints.verify(toCheck, sol)
  }

  private def runDiffSolver(diffExperiment: Experiment, toCheck: List[Experiment]) {
    CEGIS.solveDiff(diffExperiment, toCheck)
  }

  private def runDifferentiation(sets: Set[Set[Experiment]]) = {
    var diffFound = false
    if (sets.isEmpty)
      terminate("Nothing to differentiate")
    else {
      val firstSet = sets.head
      for (otherSet <- sets - firstSet) {
        if (firstSet != otherSet) {
          diffFound = true
          log("Outcome sets differ!")
          log("First set:")
          log(firstSet.mkString("\n"))
          log("Second set:")
          log(otherSet.mkString("\n"))
        }
      }
      if (!diffFound) log("Outcomes don't differ.")
    }
  }

  private def runSimplification(toCheck: List[Experiment]) = {
    Summaries.summarize(Settings.solution, toCheck, 0)
  }

  private def runSummarization(toCheck: List[Experiment]) = {
    TraceSummarization.summarizeExperiments(toCheck, Settings.solution)
  }

  private def runSimply(toCheck: List[Experiment]) = {
    for (exp <- toCheck) {
      Settings.runningMethod(exp, Settings.coarseSchedule, Settings.solution) match {
        case Some(_) =>
        case None => noErrors = false
      }
    }
  }

  private def runFateCollection(toCheck: List[Experiment]) = Settings.solution match {
    case Some(sol) => {
      var collection = Set[Experiment]()
      for (exp <- toCheck) {
        val allOutcomes = CEGIS.findAllOutcomes(exp, sol)
        log("All outcomes for " + exp)
        log(allOutcomes.size.toString + " total:")
        log(allOutcomes.map(_.mkString("")).mkString("\n"))
        val expWithOutcomes = exp.copy(fates = allOutcomes)
        collection += expWithOutcomes
      }
      val fname = "collection-" + runTimeStamp
      writeToFile(fname, Serialization.serialize(collection))
      val linkName = "last-collection"
      makeLink(fname, linkName)
    }
    case None => {
      logWarning("Running fate collection without solution.")
    }
  }

  private def runPruning(referenceOutcomes: Set[Experiment]) = {
    // try to remove one experiment at a time from ref outcomes
    // can you synthesize some model that differs on that outcome?
    // if yes, do not prune that experiment
    // if no, remove it from references
    var canBePruned = Set[Experiment]()
    var cannotBePruned = Set[Experiment]()
    while (referenceOutcomes != canBePruned ++ cannotBePruned) {
      val stillToConsider = referenceOutcomes -- (canBePruned ++ cannotBePruned)
      val candidate = stillToConsider.head
      val canSynthesize = 
        CEGIS.solveDiff(candidate, ((referenceOutcomes -- canBePruned) - candidate).toList)
      if (canSynthesize) {
        cannotBePruned += candidate
        log("Not pruned: " + candidate)
      } else {
        canBePruned += candidate
        log("Pruned: " + candidate)
      }
    }

    log("Summary of pruning:")
    log("Local minimum for specs:")
    log(cannotBePruned.mkString("  ", "\n  ", ""))
    log("Was pruned:")
    log(canBePruned.mkString("  ", "\n  ", ""))
  }

  private def summarizeSeenOutcomes(seen: MutableMap[Experiment, Set[Seq[String]]]) {
    for ((exp, outcomes) <- seen) {
      log("Seen outcomes for experiment " + exp)
      log(outcomes.size.toString + " distinct patterns:")
      log(outcomes.map(_.mkString(" ")).mkString("\n"))
    }
  }

  /** process command-line options */
  private def processOptions(options: List[String]) {
    for (option <- options) {
      option match {
        case s if s == "gui" => {
          Settings.showGUI = true
        }
        case s if s == "fullsize" => {
          Settings.resizeImages = false
        }
        case s if s.startsWith("cells") => {
          val List(idx1, idx2) = s.substring("cells=".length).split(":").toList
          Settings.cellsToShow = (idx1.toInt - 1, idx2.toInt - 1)
        }
        case s if s.startsWith("ratio") => {
          val ratio = s.substring("ratio=".length).toDouble
          Settings.resizeRatio = ratio
        }
        case s if s.startsWith("silent") => {
          Settings.verbose = false
        }
        case s if s.startsWith("random") => {
          val nb = s.substring("random=".length).toInt
          Settings.setRunningMethod(RandomTest(nb))
        }
        case s if s.startsWith("mutations") => {
          val indices = s.substring("mutations=".length).split(":").toList
          indices match {
            case List(idx1, idx2) => 
              Settings.mutations = Some((idx1.toInt - 1, idx2.toInt - 1))
            case List(idx) =>
              val i = idx.toInt - 1
              Settings.mutations = Some((i, i))
            case _ => sys.error("Bad mutation index")
          }
        }
        case s if s.startsWith("schedule") => {
          val param = s.substring("schedule=".length)
          if (param == "random") {
            Settings.setCoarseSchedule(randomMacroStepSchedule(
              Model.nbAsyncCells, Settings.runLength))
          } else {
            val sched = for (line <- scala.io.Source.fromFile(param).getLines) yield {
              val channelValues = line.split(" ").toList
              channelValues map {
                case ">" => EnabledRight
                case "<" => EnabledLeft
                case "-" => Disabled
                case _ => sys.error("bad schedule file")
              }
            }
            Settings.setCoarseSchedule(sched.toList)
          }
        }
        case s if s == "solve" => {
          Settings.setRunningMethod(Solve)
        }
        case s if s == "verify" => {
          Settings.setRunningMethod(Verify)
        }
        case s if s startsWith "solvediff" => {
          val fname = s.substring("solvediff=".length)
          val content = readFromFile(fname)
          val recovered = Serialization.deserialize[Set[Experiments.Experiment]](content)
          Settings.setRunningMethod(SolveForDifferentOutcome(recovered))
        }
        case s if s.startsWith("enum") => {
          Settings.nbMaxModels = s.substring("enum=".length).toInt
        }
        case s if s.startsWith("diff") => {
          val fileNames = s.substring("diff=".length)
          val sets = for (fname <- fileNames.split(":")) yield {
            val content = readFromFile(fname)
            val recovered = Serialization.deserialize[Set[Experiment]](content)
            recovered
          }
          Settings.setRunningMethod(CompareOutcomeSets(sets.toSet))
        }
        case s if s.startsWith("checks") => {
          Settings.nbChecks = s.substring("checks=".length).toInt
        }
        case s if s == "concrete" => {
          Settings.useHoles = false
        }
        case s if s.startsWith("solution") => {
          val fname = s.substring("solution=".length)
          val content = readFromFile(fname)
          val recovered = Serialization.deserialize[Solution](content)
          Settings.solution = Some(recovered)
        }
        case s if s.startsWith("initset") => {
          val fname = s.substring("initset=".length)
          val content = readFromFile(fname)
          val recovered = 
            Serialization.deserialize[Set[(CoarseSchedule, Experiments.Experiment)]](content)
          Settings.initialInput = Some(recovered)
        }
        case s if s == "useEvaluator" => {
          Settings.runningMethod = Interpreter.run _
        }
        case s if s.startsWith("bv") => {
          Settings.bitvectorWidth = Some(s.substring("bv=".length).toInt)
        }
        case s if s == "simplify" => {
          Settings.setRunningMethod(SimplifyAutomata)
        }
        case s if s == "summarize" => {
          Settings.setRunningMethod(SummarizeTraces)
        }
        case s if s == "collect" => {
          Settings.setRunningMethod(CollectFates)
        }
        case s if s startsWith "prune" => {
          val fname = s.substring("prune=".length)
          val content = readFromFile(fname)
          val recovered = Serialization.deserialize[Set[Experiments.Experiment]](content)
          Settings.setRunningMethod(PruneExperiments(recovered))
        }
        case s if s startsWith "runlength" => {
          Settings.runLength = s.substring("runlength=".length).toInt
        }
        case _ => terminate("Invalid option: " + option)
      }
    }
  }
}
