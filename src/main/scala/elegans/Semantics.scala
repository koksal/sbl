package elegans

import Cells._
import Constraints._
import z3.scala._

trait Semantics {
  /** Perform initial work with cells */
  def initializeConstraints(cells: List[Cell], scheduleLength: Int): Unit

  /** Finalize constraints before solver invocation */
  def finalizeConstraints(): Unit

  /** Reset internal state in semantics */
  def restart(): Unit

  /** Extract synthesis solution from model */
  def solution(model: Z3Model): Solution

  /** Given the global solution instance, concretize cells using the given
   * solution */
  def concretize(cells: Seq[Cell], solution: Solution): Unit
}

trait Logic {
  /** Enclosing node */
  private var node: Node = null
  def setNode(n: Node) = {
    node = n
  }

  /** Assert constraints for deciding output values associated with this logic
   * */
  def assertLogic(t: Int): Unit

  /** Take one step in concrete evaluation */
  def act(): Unit

  /** Assert initial output port values */
  def assertInitialOutputValues(): Unit

  def toDotString : String

  /** Logic id is handled by cell upon declaration */
  var id: String = ""
}

trait LogicSolution
