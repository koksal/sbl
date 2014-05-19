package elegans

import Experiments._
import Schedules._
import Model._
import Constraints._
import Cells._

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}
import scala.collection.mutable.ListBuffer

object Interpreter {

  val runProfiler = new Stopwatch("interpreter")
  val concretizationProfiler = new Stopwatch("concretization")

  def run(experiment: Experiment, schedule: List[MacroStep], 
      solution: Option[Solution]): Option[(Seq[Cell], History)] = {
    runProfiler.start
    var runOK = true
    val candidateHistory: History = MutableMap[Int, MutableMap[Port, Boolean]]()

    val (nonVPCs, aPrioriChannels, vpcs) = createSystem(experiment)

    // log("Running " + experiment)

    try {
      val allCells = nonVPCs ::: vpcs

      solution match {
        case Some(sol) => 
          concretizationProfiler.start
          concretize(allCells, sol)
          concretizationProfiler.stop
        case None =>
      }

      allCells foreach (_.sanityCheck())

      val visualizer = new Visualization.Visualizer()

      candidateHistory(0) = portValues(allCells)
      val initSteps = initialSteps(nonVPCs, aPrioriChannels, vpcs)

      for (step <- initSteps)
        takeStep(allCells, step)

      for ((macrostep, i) <- schedule zipWithIndex) {
        val microsteps = interleaving(macrostep)
        for (microstep <- microsteps) {
          val stepForAllCells = (nonVPCs map (_ => true)) ::: microstep

          if (Settings.showGUI)
            visualizer.microSnapshot(allCells, stepForAllCells)

          takeStep(allCells, stepForAllCells)
        }

        candidateHistory(i + 1) = portValues(allCells)
      }

      if (Settings.showGUI)
        visualizer.microSnapshot(allCells, Nil)

      // assert correct fate decisions
      throw new Exception("FATE DECISION CHECKS NEED TO BE REVISED.")

      // log("  Decided: " + (vpcs map decidedFate).mkString(", "))

      if (Settings.showGUI)
        visualizer.show()

      if (!runOK) {
        logWarning("Failed check for " + experiment)
      }
      runProfiler.stop

      if (runOK) Some((allCells, candidateHistory)) else None
    } catch {
      case UndefinedHole(id) =>
        logWarning("Hole " + id + " was undefined for " + experiment)
        None
    }
  }

  private def initialSteps(nonVPCs: List[Cell], aPrioriChannels: List[(Cell, Cell)], 
      vpcs: List[Cell]): List[MicroStep] = {
    val toHandle = MutableSet() ++ nonVPCs

    val result = ListBuffer[MicroStep]()

    def handle() {
      if (!toHandle.isEmpty) {
        val canTakeStep = nonVPCs.filter(cell => ! toHandle.exists(
          otherCell =>
            aPrioriChannels.contains((otherCell, cell))
          )
        )

        toHandle --= canTakeStep
        val nextStep = (nonVPCs map (canTakeStep contains _)) ::: (vpcs map (_ => false))
        result append nextStep
        handle()
      }
    }

    handle()

    result.toList
  }

  /** Take step:
   *  The goal is to compute "next" values of ports.
   *    - for input values, read either current or next value of output ports,
   *      depending on nature of edges
   *    - for output values, read next values of input ports.
   */
  private def takeStep(cells: List[Cell], ms: MicroStep) {
    for ((cell, shouldMove) <- cells zip ms) {
      if (shouldMove) {
        for (n <- cell.orderedN) {
          // input ports
          for (ip <- n.inputPorts) {
            setInputPortNextValue(ip)
          }

          // output ports
          for (l <- n.logics) {
            l.act()
          }
        }
      } else {
        // do nothing.
      }
    }

    // now update values.
    for ((cell, shouldMove) <- cells zip ms) {
      if (shouldMove) {
        cell.updateValues()
      }
    }
  }

  private def setInputPortNextValue(ip: Port) {
    var nextValue = false
    for (de <- ip.incomingDelayedEdges) {
      val sourcePort = de.source
      nextValue = nextValue || sourcePort.enabledNow
    }
    for (nde <- ip.incomingNonDelayedEdges) {
      val sourcePort = nde.source
      nextValue = nextValue || sourcePort.enabledNext
    }
    ip.setNextValue(nextValue)
  }

  private def portValues(cells: List[Cell]): MutableMap[Port, Boolean] = {
    val res = MutableMap[Port, Boolean]()
    for (c <- cells) {
      for (n <- c.N) {
        for (p <- n.P) {
          res(p) = p.enabledNow
        }
      }
    }
    res
  }
}
