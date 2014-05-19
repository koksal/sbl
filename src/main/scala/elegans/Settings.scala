package elegans

import Schedules._
import Experiments._
import RunningMethods._

object Settings {
  var verbose: Boolean = true

  // visualization parameters
  var showGUI: Boolean = false
  var resizeImages: Boolean = true
  var resizeRatio: Double = .75
  var cellsToShow: (Int, Int) = (0, 5)

  var mutations: Option[(Int,Int)] = None
  var runLength: Int = 10

  private var _coarseSchedule: CoarseSchedule = null

  def coarseSchedule: CoarseSchedule = {
    if (_coarseSchedule == null) {
      _coarseSchedule = syncCoarseSchedule(Model.nbAsyncCells, runLength)
      _coarseSchedule
    } else {
      assert(_coarseSchedule.size == runLength)
      _coarseSchedule
    }
  }

  def setCoarseSchedule(cs: CoarseSchedule) = {
    _coarseSchedule = cs
  }

  var nbMaxModels: Int = 1
  var useHoles: Boolean = true
  var nbChecks: Int = 0
  var runningMethod: ((Experiment, CoarseSchedule, Option[Constraints.Solution]) => 
      Option[(Seq[Cells.Cell], Constraints.History)]) =
    Constraints.runSymbolic _
  var bitvectorWidth: Option[Int] = None
  var solution: Option[Constraints.Solution] = None
  var initialInput: Option[Set[(CoarseSchedule, Experiment)]] = None

  var toPerform: RunningMethod = Run
  def setRunningMethod(m: RunningMethod) {
    if (toPerform != Run) {
      terminate("More than one running method specified.")
    } else {
      toPerform = m
    }
  }
}
