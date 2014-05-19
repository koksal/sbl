package elegans

import Cells._
import Schedules._
import Constraints._
import Experiments._
import Interpreter._
import Model._

object CEGIS {
  case class CexException(sched: CoarseSchedule, exp: Experiment) extends Exception

  private def saveInputSet(is: Set[(CoarseSchedule, Experiment)], index: Int) {
    val fileName = runTimeStamp + "-input-" + index
    val folderName = "inputs"
    val pathName = folderName + "/" + fileName
    testAndMakeFolder(folderName)
    writeToFile(pathName, Serialization.serialize(is))
    log("Input set written to " + pathName)
  }

  /** Finds all outcomes that can be seen by running the specified experiment */
  def findAllOutcomes(experiment: Experiment, solution: Solution): Set[Seq[String]] = {
    // start populating fate set with outcome on sync. schedule
    val syncSched = Schedules.syncCoarseSchedule(Model.nbAsyncCells, 
      Settings.runLength)
    Settings.runningMethod(experiment, syncSched, Some(solution)) match {
      case Some((cells, trace)) => {
        val decided = decidedFates(cells, trace)
        var allFates = Set[Seq[String]](decided)
        var collectedAllFates = false

        while (!collectedAllFates) {
          var experimentWithAllOutcomes = experiment.copy(fates = allFates)
          Constraints.verify(experimentWithAllOutcomes, solution) match {
            case Some(cexSched) => {
              Settings.runningMethod(experiment, cexSched, Some(solution)) match {
                case Some((newCells, newTrace)) => {
                  val toAdd = decidedFates(newCells, newTrace)
                  log("Adding new fate to collection: " + toAdd.mkString(""))
                  allFates += toAdd
                }
                case None => {
                  terminate("Run failed for finding outcome from cex schedule!")
                }
              }
            }
            case None => {
              collectedAllFates = true
            }
          }
        }

        allFates
      }
      case None => {
        terminate("Run failed for finding synchronous outcome!")
      }
    }
  }

  /** Finds two models that differ in their outcome for the given experiment */
  def solveByDifferentiating(experiments: List[Experiment], experimentToDiff: Experiment): Option[(Solution, Solution)] = {
    // get one solution
    val solutions = solve(experiments, 1)
    
    if (solutions.size == 0) {
      logWarning("Cannot find any model")
      None
    } else {
      log("Found first model, now searching for the second.")
      val firstSolution = solutions.head

      val syncSched = Schedules.syncCoarseSchedule(Model.nbAsyncCells,
        Settings.runLength)

      // remove the outcome on an execution of the experiment to diff
      Settings.runningMethod(experimentToDiff, syncSched, Some(firstSolution)) match {
        case Some((cells, trace)) => {
          val decided = decidedFates(cells, trace)
          log("Decided fate:")
          log(decided.mkString(" "))
          val toAvoid = (experimentToDiff.copy(
            fates = Set(decided)
          ), Some(syncSched))

          val newSolutions = solve(experiments, 1, 
            toAvoidInSomeRun = Set(toAvoid))
          if (newSolutions.size == 0) {
            logWarning("Cannot find model with different outcome")
            None
          } else {
            val secondSolution = newSolutions.head
            Some((firstSolution, secondSolution))
          }
        }
        case None => terminate("Should not have happened.")
      }


    }
  }

  def solveDiff(
      expToDiff: Experiment,
      experiments: List[Experiment]): Boolean = {
    val outcomeSet = expToDiff.fates

    // first try to synthesize a model that decides an additional fate
    val solSet = solve(experiments, 1, toAvoidInSomeRun = Set((expToDiff, None)))
    if (solSet.isEmpty) {
      // could not synthesize a model that decides more
      // we can try fewer
      log("Could not find model that decides an unseen fate.")
      var foundSmallerSet = false
      if (expToDiff.fates.size > 1) {
        for (outcome <- expToDiff.fates; if !foundSmallerSet) {
          log("Trying to synthesize a model that does not decide:")
          log(outcome)
          val modifiedExpToDiff = 
            expToDiff.copy(fates = expToDiff.fates - outcome)
          val augmentedExp = experiments ::: List(modifiedExpToDiff)
          val newSolSet = solve(augmentedExp, 1, excludedFromNondeterminismCheck = Set(modifiedExpToDiff))
          if (newSolSet.isEmpty) {
            log("No solutions excluding " + outcome)
          } else {
            log("Found a new model that avoids " + outcome)
            foundSmallerSet = true
          }
        }
      }
      foundSmallerSet
    } else {
      log("Found a new model that decides more fates on experiment:")
      log(expToDiff)
      true
    }
  }

  def solve(
      experiments: List[Experiment], 
      nbMaxModels: Int,
      toSeeInSomeRun:   Set[(Experiment, Option[CoarseSchedule])] = Set(),
      toAvoidInSomeRun: Set[(Experiment, Option[CoarseSchedule])] = Set(),
      excludedFromNondeterminismCheck: Set[Experiment] = Set()): Set[Solution] = {
    var continue = true

    var nondetCexToSeeInSomeRun = toSeeInSomeRun

    // // start with a non-GF experiment and a GF one
    val wildtypeLin12Exp  = experiments.find(_.mutations.get("lin12") match {
      case Some("wt") => true
      case _ => false
    })
    val gfLin12Exp        = experiments.find(_.mutations.get("lin12") match {
      case Some("gf") => true
      case _ => false
    })

    var vpcStudyInputs = 
        (wildtypeLin12Exp.toSet ++ gfLin12Exp.toSet) map (exp => (Settings.coarseSchedule, exp))

    var inputPairs: Set[(CoarseSchedule, Experiment)] =
      if (vpcStudyInputs.isEmpty) Set((Settings.coarseSchedule, experiments.head))
      else vpcStudyInputs

    var verifiedSolutions = Set[Solution]()
    var cegisLoopCounter = 0

    val experimentsForNondeterminismCheck = (experiments.toSet -- excludedFromNondeterminismCheck).toList

    while (continue) {
      cegisLoopCounter += 1
      // saveInputSet(inputPairs, cegisLoopCounter)

      val fromSynthesizer = Constraints.synthesize(inputPairs, nondetCexToSeeInSomeRun, toAvoidInSomeRun) 
      fromSynthesizer match {
        case Some(solution) =>
          log("Found candidate!")
          findCounterexample(inputPairs map (_._1), experiments, solution) match {
            case Some((cexSched, cexExp)) => {
              inputPairs = inputPairs + ((cexSched, cexExp))
            }
            case None => {
              // need to verify that nondeterministic outcomes are reached
              findNonReachableOutcome(experimentsForNondeterminismCheck, solution) match {
                case Some(cexOutcome) => {
                  // add nonreachable outcome to accumulated set
                  nondetCexToSeeInSomeRun += ((cexOutcome, None))
                }
                case None => {
                  log("Model verifies nondeterministic results!")
                  verifiedSolutions += solution
                  val howManyToFind = nbMaxModels - verifiedSolutions.size

                  assert(howManyToFind >= 0)
                  if (howManyToFind > 0) {
                    val candidates = Constraints.enumerateForInput(inputPairs, 
                      nondetCexToSeeInSomeRun, toAvoidInSomeRun, howManyToFind)

                    var continueVerifying = true
                    for ((candidate, index) <- candidates zipWithIndex; if continueVerifying) {
                      log("Testing candidate...")
                      findCounterexample(inputPairs map (_._1), experiments, candidate) match {
                        case Some((cexSched, cexExp)) =>
                          logWarning("Spurious candidate!")
                          inputPairs += ((cexSched, cexExp))
                          continueVerifying = false
                        case None =>
                          // does candidate also verify nondeterministic outcomes?
                          findNonReachableOutcome(experimentsForNondeterminismCheck, candidate) match {
                            case Some(cexOutcome2) => {
                              // add nonreachable outcome to accumulated set
                              nondetCexToSeeInSomeRun += ((cexOutcome2, None))
                            }
                            case None => {
                              logSuccess("Verified candidate!")
                              verifiedSolutions += candidate
                            }
                          }
                      }
                    }
                  }

                  val howManyAreMissing = nbMaxModels - verifiedSolutions.size
                  if (howManyAreMissing == 0) {
                    log("Enumerated " + nbMaxModels + " model(s)!")
                    continue = false
                  } else {
                    log("Need to enumerate " + howManyAreMissing + " mode models!")
                  }
                
                }
              }
            }
          }
        case None => 
          log("No solution candidates!")
          continue = false
      }
    }

    for ((s, index) <- verifiedSolutions zipWithIndex) {
      val folderName = "models"

      def writeVPC(e: Option[Experiment], suffix: String) = e match {
        case Some(e) =>
          val vpcString = concreteVPCString(s, e)
          val filename = "model-" + suffix + "-" + runTimeStamp + "-" + index
          val pathName = folderName + "/" + filename
          testAndMakeFolder(folderName)
          writeToFile(pathName, vpcString)
          log("Model for " + suffix + " written to file: " + pathName)
          val linkName = "last-" + suffix + "-model"
          makeLink(pathName, linkName)
        case None =>
      }

      def writeSerialized() {
        val fname = "solution-" + runTimeStamp + "-" + index
        val pathName = folderName + "/" + fname
        writeToFile(pathName, Serialization.serialize(s))
        log("Serialized solution written to " + pathName)
        val linkName = "last-solution"
        makeLink(pathName, linkName)
      }

      def drawTransition(e: Option[Experiment], suffix: String) = e match {
        case Some(e) =>
          var dots = getTransitionDotFormat(s, e)
          for (dot <- dots){
            // println(dot)
          }
        case None =>
      }

      val wildtypeLin12Exp  = experiments.find(_.mutations.get("lin12") match {
        case Some("wt") => true
        case _ => false
      })
      val gfLin12Exp        = experiments.find(_.mutations.get("lin12") match {
        case Some("gf") => true
        case _ => false
      })

      writeVPC(wildtypeLin12Exp, "wt")
      writeVPC(gfLin12Exp, "gf")
      writeVPC(Some(experiments.head), "first-experiment")
      writeSerialized()
      
      // Summaries.summarize(Some(s), experiments, index)

    }

    verifiedSolutions
  }

  private def findNonReachableOutcome(experiments: List[Experiment], 
      solution: Solution): Option[Experiment] = {

    log("Attempting to verify lower bound for specification.")
    var toRet: Option[Experiment] = None
    for (exp <- experiments; if !toRet.isDefined) {
      val allOutcomes = findAllOutcomes(exp, solution)
      val nonReached = exp.fates -- allOutcomes
      if (!nonReached.isEmpty) {
        log("Found at least one nonreachable outcome!")
        log("What hasn't been reached:")
        log(nonReached.mkString("\n"))
        val fatePatternToAdd = nonReached.head
        log("Will add:")
        log(fatePatternToAdd)
        toRet = Some(exp.copy(fates = Set(fatePatternToAdd)))
      } else {
        log("All fates reached for " + exp)
      }
    }

    toRet
  }

  private def findCounterexample(initialSchedules: Set[CoarseSchedule], 
    experiments: List[Experiment], sol: Solution): Option[(CoarseSchedule, Experiment)] = {
    var cex: Option[(CoarseSchedule, Experiment)] = None

    try {
      // first try schedules in hand

      // for ((s, i) <- initialSchedules zipWithIndex) {
      //   findByRunning(s, experiments, sol, i + 1)
      // }

      // last and not least, verify solution in hand
      log("Now starting verification.")
      for (exp <- experiments) {
        findBySolving(exp, sol)
      }
      log("Verified all experiments!")
    } catch {
      case CexException(sch, exp) => 
        cex = Some((sch, exp))
    }

    cex
  }

  private def findByRunning(sched: CoarseSchedule, experiments: List[Experiment], 
      sol: Solution, attempt: Int) {
    for (exp <- experiments) {
      runSymbolic(exp, sched, Some(sol)) match {
        case Some(_) =>
        case None => {
          log("Found counterexample schedule at attempt " + attempt + "!")
          log((sched map (step => step.mkString(" "))).mkString("\n"))
          throw new CexException(sched, exp)
        }
      }
    }
  }

  private def findBySolving(exp: Experiment, sol: Solution) {
    try {
      verify(exp, sol) match {
        case Some(sched) =>
          log("Found counterexample while attempting verification.")
          log(exp)
          log((sched map (step => step.mkString(" "))).mkString("\n"))
          throw new CexException(sched, exp)
        case None =>
      }
    } catch {
      case e: UndefinedHole => 
        throw new CexException(Settings.coarseSchedule, exp)
    }
  }

  private def concreteVPCString(sol: Solution, exp: Experiment): String = {
    val (nonvpcs, aPrioriChannels, vpcs) = createSystem(exp)
    concretize(nonvpcs ++ vpcs, sol)
    ((vpcs ::: nonvpcs) map (CellPrinter(_))).mkString("\n\n")
  }

  private def getTransitionDotFormat(sol: Solution, exp: Experiment) : Seq[String] = {
    val (nonvpcs, aPrioriChannels, vpcs) = createSystem(exp)
    concretize(nonvpcs ++ vpcs, sol)
    CellDotPrinter(vpcs(3))
  }
}
