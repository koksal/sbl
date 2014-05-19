package elegans

import Experiments.Experiment

object RunningMethods {
  sealed trait RunningMethod
  case object Run extends RunningMethod
  case class RandomTest(nbTests: Int) extends RunningMethod
  case object Verify extends RunningMethod
  case object Solve extends RunningMethod
  case class SolveForDifferentOutcome(toDiff: Set[Experiment]) extends RunningMethod
  case class PruneExperiments(toPrune: Set[Experiment]) extends RunningMethod
  case class CompareOutcomeSets(outcomeSets: Set[Set[Experiment]]) extends RunningMethod
  case object SimplifyAutomata extends RunningMethod
  case object SummarizeTraces extends RunningMethod
  case object CollectFates extends RunningMethod
}
