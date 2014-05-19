package elegans

import Cells._
import Constraints._
import z3.scala._

object BooleanStatelessSemantics extends Semantics {
  val booleanSolutionKey = "booleanSolution"

  /** Perform initial work with cells */
  def initializeConstraints(cells: List[Cell], scheduleLength: Int): Unit = { }

  /** Finalize constraints before solver invocation */
  def finalizeConstraints(): Unit = { }

  /** Reset internal state in semantics */
  def restart(): Unit = { }

  /** Extract synthesis solution from model */
   def solution(model: Z3Model): Solution = Map(booleanSolutionKey -> new BooleanStatelessSolution())

  /** Given the global solution instance, concretize cells using the given
   * solution */
  def concretize(cells: Seq[Cell], solution: Solution): Unit = { }

  /** Data structures for describing node logic */
  sealed trait Gate
  case object TrueGate extends Gate
  case object FalseGate extends Gate
  case class LeafGate(p: Port) extends Gate
  case class AndGate(gs: Seq[Gate]) extends Gate
  case class OrGate(gs: Seq[Gate]) extends Gate
  case class NotGate(g: Gate) extends Gate

  implicit def portBundle2gate(pb: PortBundle): LeafGate = {
    assert(pb.ports.size == 1)
    LeafGate(pb.ports.head)
  }

  def gate2ast(gate: Gate, time: Int): Z3AST = gate match {
    case TrueGate => ctx.mkTrue
    case FalseGate => ctx.mkFalse
    case LeafGate(p) => portVars(time)(p)
    case AndGate(gs) => ctx.mkAnd((gs map (g => gate2ast(g, time))): _*)
    case OrGate(gs) => ctx.mkOr((gs map (g => gate2ast(g, time))): _*)
    case NotGate(g) => ctx.mkNot(gate2ast(g, time))
  }
}

import BooleanStatelessSemantics._

case class BooleanStatelessLogic(outBundle: PortBundle, gate: Gate) extends Logic {
  private val out = {
    assert(outBundle.ports.size == 1)
    outBundle.ports.head
  }

  /** Assert constraints for deciding output values associated with this logic
   * */
  def assertLogic(t: Int): Unit = {
    assertConstraint(ctx.mkIff(portVars(t)(out), gate2ast(gate, t)))
  }

  /** Take one step in concrete evaluation */
  def act(): Unit = throw TODOException

  /** Assert initial output port values */
  def assertInitialOutputValues(): Unit = {
    // set all ports to false at timestep 0
    assertConstraint(ctx.mkNot(portVars(0)(out)))
  }

  def toDotString : String = "gate logic dot string not implemented"
}

class BooleanStatelessSolution extends LogicSolution
