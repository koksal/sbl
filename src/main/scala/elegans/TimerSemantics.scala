package elegans

import Cells._
import Constraints._
import TimerLogic._
import TimerSemantics._
import z3.scala._
import z3.scala.dsl._

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

object TimerSemantics extends Semantics {
  /** Perform initial work with cells */
  def initializeConstraints(cells: List[Cell], scheduleLength: Int): Unit = {
    makeTriggerVars(cells, scheduleLength)
    disableInitialTriggers(cells)
    assertTriggers(cells, scheduleLength)
  }

  /** Finalize constraints before solver invocation */
  def finalizeConstraints(): Unit = {
    assertHoleEqualities()
  }

  /** Reset internal state in semantics */
  def restart(): Unit = {
    holes = MutableMap[(List[Port], String), HoleExpression]()
    triggers = MutableMap[Int, MutableMap[Logic, Z3AST]]()
  }

  /** Extract synthesis solution from model */
  def solution(model: Z3Model): Solution = {
    val recovered = recoverDNF(model)
    val ret = for ((id, lst1) <- recovered) yield {
      (id, TimerSolution(qmm(lst1)))
    }
    ret
  }

  /** Given the global solution instance, concretize logic instances 
   * using the given solution */
  def concretize(cells: Seq[Cell], solution: Solution): Unit = {
    for (c <- cells)
      for (n <- c.N)
        for (tl @ TimerLogic(_, _, _) <- n.logics)
          tl.concretize(solution)
  }

  private def makeTriggerVars(cells: List[Cell], scheduleLength: Int): Unit = {
    for (idx <- 0 to scheduleLength) {
      triggers(idx) = triggers.getOrElse(idx, MutableMap())
      for (cell <- cells) {
        for (node <- cell.N) {
          for (tl @ TimerLogic(out, _, _) <- node.logics) {
            triggers(idx)(tl) = ctx.mkFreshBoolConst(out.toString + "_trigger_" + idx)
          }
        }
      }
    }
  }

  private def disableInitialTriggers(cells: List[Cell]): Unit = {
    for (c <- cells)
      for (n <- c.N)
        for (tl @ TimerLogic(_, _, _) <- n.logics)
          assertConstraint(!triggers(0)(tl))
  }

  private def assertTriggers(cells: List[Cell], scheduleLength: Int) {
    for (t <- 1 to scheduleLength) {
      for (cell <- cells) {
        for (n <- cell.N) {
          for (tl @ TimerLogic(_, triggerPred, _) <- n.logics) {
            val value = ctx.mkOr(triggers(t - 1)(tl), pred2ast(triggerPred, t))
            assertConstraint(ctx.mkIff(triggers(t)(tl), value))
          }
        }
      }
    }
  }

  private var triggers    = MutableMap[Int, MutableMap[Logic, Z3AST]]()
  private var holes       = MutableMap[(List[Port], String), HoleExpression]()

  class HoleExpression(val ports: List[Port]) {
    def allCombinations(l: List[Predicate]): List[List[Predicate]] = l match {
      case Nil => List(List())
      case p :: ps =>
        val rest = allCombinations(ps)
        val l1 = rest map (rs => !p :: rs)
        val l2 = rest map (rs =>  p :: rs)
        l1 ::: l2
    }
    
    private val cache = MutableMap[Int, Z3AST]()
    val tableRows = allCombinations(ports.toList map (p => LeafPredicate(p)))
    val tableOutputs = for (row <- tableRows) yield
      ctx.mkFreshBoolConst(row.mkString("_"))

    private def mkAST(t: Int) = {
      // create fresh var, assert it is equal to the translation of hole,
      // return the var.
      val rowsAtT = tableRows map (row => row map (p => pred2ast(p, t)))
      val res = ctx.mkFreshBoolConst("hole_" + ports.mkString("_") + "_" + t)
      for ((row, out) <- rowsAtT zip tableOutputs) {
        val implication = ctx.mkImplies(ctx.mkAnd(row: _*), ctx.mkIff(out, res))
        assertConstraint(implication)
      }
      res
    }

    def ast(t: Int): Z3AST = cache.get(t) match {
      case Some(tree) => tree
      case None =>
        val newAST = mkAST(t)
        cache += (t -> newAST)
        newAST
    }
  }

  private def mkHoleExpr(ps: List[Port], idString: String, step: Int): Z3AST = {
    holes.get((ps, idString)) match {
      case Some(he) => he.ast(step)
      case None => 
        val newExpr = new HoleExpression(ps)
        holes += ((ps, idString) -> newExpr)
        newExpr.ast(step)
    }
  }

  private def assertHoleEqualities() {
    val perIdString = holes.groupBy(he => he._1._2) // group by string id of hole
    for (group <- perIdString) {
      group match {
        case (id, holes) =>
          for (he <- holes) {
            // println("HOLE")
            // println(he)
            for ((o1, o2) <- he._2.tableOutputs zip holes.head._2.tableOutputs)
              assertConstraint(ctx.mkIff(o1, o2))
          }
      }
    }
  }

  private def recoverDNF(model: Z3Model) = {
    val perIdString = holes.groupBy(he => he._1._2) // group by string id of hole
    val dnfForms = for (group <- perIdString) yield {
      group match {
        case (id, holes) =>
          val he = holes.head
          val conjunctions = for ((out, row) <- he._2.tableOutputs zip he._2.tableRows;
            if model.evalAs[Boolean](out) == Some(true)) yield {
            row.map{
              case LeafPredicate(port) => true
              case NotPredicate(LeafPredicate(port)) => false
              case _ => terminate("invalid predicate")
            }
          }
          val ret = conjunctions.toList
          (id, ret)
      }
    }
    dnfForms
  }

  // Simplify minterms using QuineMcCluskey for simpler output
  // TODO doesn't simplify as of now.
  private def qmm(minterms: List[List[Boolean]]): List[List[Option[Boolean]]] = {
    for (minterm <- minterms) yield {
      minterm map {
        v => Some(v)
      }
    }
  }

  /** Instantiates a constraints for a predicate at a given timestep */
  def pred2ast(pred: Predicate, step: Int): Z3AST = pred match {
    case TruePredicate => ctx.mkTrue
    case FalsePredicate => ctx.mkFalse
    case LeafPredicate(port) => portVars(step)(port)
    case NotPredicate(p) => ctx.mkNot(pred2ast(p, step))
    case OrPredicate(p1, p2) => ctx.mkOr(pred2ast(p1, step), pred2ast(p2, step))
    case AndPredicate(p1, p2) => ctx.mkAnd(pred2ast(p1, step), pred2ast(p2, step))
    case HolePredicate(ps, str) => mkHoleExpr(ps, str, step)
  }

  def triggeredAt(l: Logic, t: Int) = {
    val activeAtLastStep = if (t == 0) ctx.mkFalse else triggers(t - 1)(l)
    ctx.mkAnd(
      ctx.mkNot(activeAtLastStep), 
      triggers(t)(l)
    )
  }

  def concretizePred(pred: Predicate, sol: Solution): Predicate = pred match {
    case HolePredicate(ports, id) =>
      sol.get(id) match {
        case Some(TimerSolution(dnf)) =>
          val clauses = for (conjunct <- dnf) yield {
            val literals = for ((port, isPositive) <- ports zip conjunct;
              if isPositive.isDefined) yield {
              val polarity = isPositive.get
              if (polarity) LeafPredicate(port) else NotPredicate(LeafPredicate(port))
            }
            literals.foldLeft[Predicate](TruePredicate){
              case (a, b) => AndPredicate(a, b)
            }
          }
          clauses.foldLeft[Predicate](FalsePredicate){
            case (a, b) => OrPredicate(a, b)
          }
        case None => throw new UndefinedHole(id)
      }
    case TruePredicate => pred
    case FalsePredicate => pred
    case LeafPredicate(p) => pred
    case NotPredicate(p) => NotPredicate(concretizePred(p, sol))
    case OrPredicate(p1, p2) => OrPredicate(concretizePred(p1, sol), concretizePred(p2, sol))
    case AndPredicate(p1, p2) => AndPredicate(concretizePred(p1, sol), concretizePred(p2, sol))
  }


}

/** A logic encodes the value of the out port at the next step based on the
 * following principles: 
 *    - its clock has been activated, and the next output value is decided by
 *      the predicate for the corresponding time step. If the value becomes
 *      true, then the edges associated with the output port are enabled, and
 *      the clock is reset.
 *    - its clock is inactive and can be activated if the trigger predicate
 *      evaluates to true. The first predicate is evaluated at the same time
 *      step, if activated.
 */
case class TimerLogic(out: Port, 
                      var trigger: Predicate, 
                      var predicates: List[Predicate]) extends Logic {

  override def toDotString: String = {
    "digraph G{ \"i need to\" -> \"implement this\"; }"
  }

  /** Assert constraints for deciding output values associated with this logic
   * */
  def assertLogic(t: Int): Unit = {
    val activeAtLastStep = if (t == 0) 
      ctx.mkFalse else portVars(t - 1)(out)
    val logicUnrolling = for ((pred, i) <- predicates zipWithIndex;
      if t - i >= 0) yield {
      
      val triggerTicked = triggeredAt(this, t - i)
      val predValid = pred2ast(pred, t)
      ctx.mkAnd(triggerTicked, predValid)
    }
    val value = ctx.mkOr(activeAtLastStep, ctx.mkOr(logicUnrolling.toList: _*))
    assertConstraint(ctx.mkIff(portVars(t)(out), value))
  }

  def concretize(sol: Solution): Unit = {
    trigger = simplify(concretizePred(trigger, sol))
    predicates = predicates map (p => simplify(concretizePred(p, sol)))
  }

  var clock = 0
  var triggered = false
  var activated = false

  var promotingOrders = Set[List[Port]]()
  var inhibitingOrders = Set[List[Port]]()

  def act() {
    var nextValue = false
    // if output port was already enabled, it stays so
    if (activated) {
      nextValue = true
    } else {
      // if clock is disabled, check if it can be enabled by the trigger
      if (!triggered && evaluateNext(trigger)) {
        // set clock to shortest fire delay that's enabled
        triggered = true
      }
      //if the clock is set, count down
      if (!activated && triggered && clock < predicates.size) {
        if (evaluateNext(predicates(clock))) {
          nextValue = true
          activated = true
        } else {
          clock += 1
        }
      }
    }

    out.setNextValue(nextValue)
  }

  def assertInitialOutputValues() {
    ctx.assertCnstr(!portVars(0)(out))
  }

  override def toString = {
    "timerlogic " + out.name + " {" + "\n" +
    indent(
      PredicatePrinter(trigger) + " => " + "\n" +
      indent(
        (for (p <- predicates) yield PredicatePrinter(p)).mkString("\n")
      )
    ) +"\n" +
    "}"
  }

  def scalaString = {
    "TimerLogic(\"" + out.name + "\")(" + PredicatePrinter(trigger) + ")(" + "\n" +
    indent(
      (for (p <- predicates) yield PredicatePrinter(p)).mkString(",\n")
    ) + "\n" +
    ")"
  }

  def promoting(ps: Port*) {
    promotingOrders += ps.toList
  }

  def inhibiting(ps: Port*) {
    inhibitingOrders += ps.toList
  }
}

object TimerLogic {
  def apply(out: PortBundle)(trigger: Predicate)(ps: Predicate*) = {
    assert(out.ports.size == 1, "Timer logic needs binary output")
    new TimerLogic(out.ports(0), trigger, ps.toList)
  }

  implicit def port2pred(p: Port): Predicate =
    LeafPredicate(p)

  implicit def bundle2pred(b: PortBundle): Predicate = {
    assert(b.ports.size == 1, "port bundle is not binary")
    LeafPredicate(b.ports(0))
  }

  implicit def bool2pred(b: Boolean): Predicate = 
    if (b) TruePredicate else FalsePredicate

  def evaluateNext(pred: Predicate) = evaluate(pred, false)

  private def evaluate(pred: Predicate, evaluateNow: Boolean): Boolean = pred match {
    case TruePredicate => true
    case FalsePredicate => false
    case LeafPredicate(port) => if (evaluateNow) port.enabledNow else port.enabledNext
    case NotPredicate(p) => !evaluate(p, evaluateNow)
    case OrPredicate(p1, p2) => evaluate(p1, evaluateNow) || evaluate(p2, evaluateNow)
    case AndPredicate(p1, p2) => evaluate(p1, evaluateNow) && evaluate(p2, evaluateNow)
    case HolePredicate(_, _) => terminate("evaluating hole predicate: " + pred)
  }

  def simplify(pred: Predicate): Predicate = pred match {
    case NotPredicate(NotPredicate(p)) => simplify(p)
    case NotPredicate(p) => NotPredicate(simplify(p))
    case OrPredicate(p1, p2) => (simplify(p1), simplify(p2)) match {
      case (FalsePredicate, sp) => sp
      case (sp, FalsePredicate) => sp
      case (TruePredicate, _) => TruePredicate
      case (_, TruePredicate) => TruePredicate
      case (sp1, sp2) => OrPredicate(sp1, sp2)
    }
    case AndPredicate(p1, p2) => (simplify(p1), simplify(p2)) match {
      case (FalsePredicate, _) => FalsePredicate
      case (_, FalsePredicate) => FalsePredicate
      case (TruePredicate, sp) => sp
      case (sp, TruePredicate) => sp
      case (sp1, sp2) => AndPredicate(sp1, sp2)
    }
    case other => other
  }

  /** Predicate syntax trees for describing logic */
  sealed trait Predicate {
    def unary_! : Predicate = NotPredicate(this)
    def ||(p: Predicate): Predicate = OrPredicate(this, p)
    def &&(p: Predicate): Predicate = AndPredicate(this, p)
  }

  case object TruePredicate extends Predicate
  case object FalsePredicate extends Predicate
  case class LeafPredicate(p: Port) extends Predicate
  case class NotPredicate(p: Predicate) extends Predicate
  case class OrPredicate(p1: Predicate, p2: Predicate) extends Predicate
  case class AndPredicate(p1: Predicate, p2: Predicate) extends Predicate
  case class HolePredicate(ps: List[Port], idString: String) extends Predicate {
    override def toString = "??" + idString
  }

  object PredicatePrinter {
    def apply(pred: Predicate): String = pred match {
      case TruePredicate => "true"
      case FalsePredicate => "false"
      case LeafPredicate(port) => port.name
      case NotPredicate(p) => "!" + enclose(p, pred)
      case OrPredicate(p1, p2) => enclose(p1, pred) + " || " + enclose(p2, pred)
      case AndPredicate(p1, p2) => enclose(p1, pred) + " && " + enclose(p2, pred)
      case HolePredicate(_, idString) => "??" + idString
    }

    private def level(p: Predicate): Int = p match {
      case TruePredicate => 0
      case FalsePredicate => 0
      case LeafPredicate(_) => 0
      case HolePredicate(_, _) => 0
      case NotPredicate(_) => 1
      case AndPredicate(_, _) => 2
      case OrPredicate(_, _) => 3
    }

    private def needsParen(child: Predicate, parent: Predicate): Boolean = {
      level(child) > level(parent)
    }

    private def enclose(child: Predicate, parent: Predicate): String = {
      if (needsParen(child, parent))
        "(" + apply(child) + ")"
      else
        apply(child)
    }
  }
}

case class TimerSolution(dnf: List[List[Option[Boolean]]]) extends LogicSolution
