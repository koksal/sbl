package elegans

import TimerLogic._

object Cells {

  def neighbors(cell: Cell): Set[Cell] = {
    var ns = Set[Cell]()
    for (n <- cell.N) {
      for (op <- n.outputPorts) {
        for (Edge(_, dst, _) <- op.E) {
          val neighbor = dst.parent.parent
          assert(neighbor != null)
          ns += neighbor
        }
      }
    }
    ns
  }

  class Cell(val name: String) {
    private var _nodes = Set[Node]()

    def N = _nodes

    /** Registers given node in cell and returns it */
    def node[N <: Node](n: N): N = {
      addNode(n)
      n.setParent(this)
      n
    }

    /** Adds a new node */
    private def addNode(n: Node) {
      _nodes += n
    }

    /** Orders nodes such that execution order is correct for nodes linked with
     * non-delay edges. Computed only once, lazily, therefore nodes should not
     * be added dynamically with this implementation */
    lazy val orderedN: List[Node] = {
      import scala.collection.mutable.{Set=>MutableSet, ListBuffer}
      val result = ListBuffer[Node]()
      val haveNonDelayedOutput = MutableSet[Node]()
      haveNonDelayedOutput ++= N.filter(n =>
        n.outputPorts.exists(p => !p.nonDelayedEdges.isEmpty))

      while(!haveNonDelayedOutput.isEmpty) {
        val noPred = haveNonDelayedOutput.find{ n1 =>
          !haveNonDelayedOutput.exists{ n2 =>
            val dpset = n2.outputPorts.map(op => op.nonDelayedEdges.map(e => e.dest))
            val dps = dpset.reduce(_ union _)
            ! (dps intersect (n1.inputPorts)).isEmpty
          }
        }.get

        result append noPred
        haveNonDelayedOutput -= noPred
      }
        

      val toReturn = result.toList ::: ((N -- result.toSet).toList)
      toReturn
    }

    def updateValues() {
      for (n <- N)
        n.updateValues()
    }

    override def toString = name

    /** Checks that 
        - there is a logic for each output port 
        - each port is connected to at least one other.
    */
    def sanityCheck() {
      for (n <- N)
        n.sanityCheck()
    }

    def intercellularInputPorts = {
      for (n <- N) {
        for (p <- n.inputPorts) {
          // todo save back edges in ports and check whether that input port is
          // in same cell

          val neighborOutputs = p.incomingEdges map (e => e.source)
        }
      }
    }

    def outcomeBundles: Map[String, Set[PortBundle]] = {
      var toRet = Map[String, Set[PortBundle]]()
      for (n <- N) {
        for ((bundle, value) <- n.outcomeBundles) {
          toRet.get(value) match {
            case Some(set) => toRet += ((value, set + bundle))
            case None => toRet += ((value, Set(bundle)))
          }
        }
      }
      toRet.toMap
    }
  }

  abstract class Node(val name: String) {
    private var _inputPorts = Set[Port]()
    private var _outputPorts = Set[Port]()
    private var _logics = Set[Logic]()
    private var _parent: Cell = null
    private var _inputBundles = Set[PortBundle]()
    private var _outputBundles = Set[PortBundle]()
    private var _outcomeBundles = Map[PortBundle, String]()

    // specific to TimerSemantics, gives a unique id to each ?? in the node
    private var holeCounter = 0
    private var logicCounter = 0

    def P: Set[Port] = _inputPorts ++ _outputPorts
    def inputPorts: Set[Port] = _inputPorts
    def outputPorts: Set[Port] = _outputPorts
    def logics = _logics
    def inputBundles = _inputBundles
    def outputBundles = _outputBundles
    def bundles = inputBundles ++ outputBundles
    def outcomeBundles = _outcomeBundles

    def parent = _parent
    def setParent(c: Cell) {
      _parent = c
    }

    def input(name: String)
             (levels: String*)
             (implicit a: Assumption = null): PortBundle = {
      val ports = levels map (l => new Port(name + "_" + l))         

      ports foreach addInput
      ports foreach (_.setParent(this))

      val bundle = PortBundle(name, ports)

      if (a != null)
        bundle.assumptions += a

      addInputBundle(bundle)
      bundle
    }

    def output(name: String)
              (levels: String*)
              (implicit a: Assumption = null): PortBundle = {
      val ports = levels map (l => new Port(name + "_" + l))         

      ports foreach addOutput
      ports foreach (_.setParent(this))

      val bundle = PortBundle(name, ports)

      if (a != null)
        bundle.assumptions += a

      addOutputBundle(bundle)
      bundle
    }

    def outcome(bundle: PortBundle, value: String) {
      _outcomeBundles += ((bundle, value))
    }

    /** Ensures that all of the same hole expressions in different
     * instantiations of each cell get the same identifier string, by using the
     * holeCounter */
    def ??(ps: Port*): HolePredicate = {
      holeCounter += 1
      HolePredicate(ps.toList, this.name + "#" + (ps.map(_.name).mkString(":")) + "_" + holeCounter) 
    }

    def logic(l: Logic): DeclaredLogic = {
      logicCounter += 1
      l.id = this.name + "#" + logicCounter.toString
      l.setNode(this)
      DeclaredLogic(l)
    }

    case class DeclaredLogic(l: Logic)

    def register(dl: DeclaredLogic) = dl match {
      case DeclaredLogic(l) =>
        _logics += l

        l match {
          case TimerLogic(out, trigger, preds) =>
            out.setLogic(l)
          case _ => 
            // doesn't need to be registered here
            // TODO refactor
        }
    }

    /** Adds a new port */
    private def addInput(p: Port) {
      _inputPorts += p
    }

    /** Adds a new port */
    private def addOutput(p: Port) {
      _outputPorts += p
    }

    private def addInputBundle(b: PortBundle) {
      _inputBundles += b
    }

    private def addOutputBundle(b: PortBundle) {
      _outputBundles += b
    }

    def updateValues() {
      for (p <- P)
        p.updateValue()
    }
    
    def sanityCheck() {
      for (op <- outputPorts) {
        // check for existence of a logic for each output
        if (op.logic.isEmpty) {
          // TODO commented because we don't have it for new semantics
          // logWarning("No logic for output port " + op.name + " in " + this.name)
        }

        // check whether it is connected to anything
        // if (op.E.isEmpty)
        //   logWarning("Output " + op + " is not connected to any other port")
      }
    }

    override def toString = parent.toString + "_" + name
  }
  
  sealed trait Assumption
  case object Monotonic extends Assumption
  case object Constant  extends Assumption

  case class PortBundle(id: String, ports: Seq[Port]) {
    var assumptions = Set[Assumption]()

    def -->(b: PortBundle) {
      if (this.ports.size != b.ports.size)
        terminate("Bundle sizes don't match for " + this.id + " --> " + b.id)
      this.ports zip b.ports foreach {
        case (p1, p2) => p1.connectDelayed(p2, Activating)
      }
    }

    def ==>(b: PortBundle) {
      if (this.ports.size != b.ports.size)
        terminate("Bundle sizes don't match for " + this.id + " --> " + b.id)
      this.ports zip b.ports foreach {
        case (p1, p2) => p1.connectNonDelayed(p2, Activating)
      }
    }

    def --|(b: PortBundle) {
      if (this.ports.size != b.ports.size)
        terminate("Bundle sizes don't match for " + this.id + " --> " + b.id)
      this.ports zip b.ports foreach {
        case (p1, p2) => p1.connectDelayed(p2, Inhibiting)
      }
    }

    def ==|(b: PortBundle) {
      if (this.ports.size != b.ports.size)
        terminate("Bundle sizes don't match for " + this.id + " --> " + b.id)
      this.ports zip b.ports foreach {
        case (p1, p2) => p1.connectNonDelayed(p2, Inhibiting)
      }
    }

  }

  class Port(val name: String) {
    private var _delayedEdges = Set[Edge]()
    private var _nonDelayedEdges = Set[Edge]()
    private var _incomingDelayedEdges = Set[Edge]()
    private var _incomingNonDelayedEdges = Set[Edge]()

    private var _parent: Node = null
    private var _logic: Option[Logic] = None

    private var _enabledNow = false
    private var _enabledNext = false

    def delayedEdges = _delayedEdges
    def nonDelayedEdges = _nonDelayedEdges
    def E = nonDelayedEdges ++ delayedEdges

    def incomingDelayedEdges = _incomingDelayedEdges
    def incomingNonDelayedEdges = _incomingNonDelayedEdges
    def incomingEdges = incomingDelayedEdges ++ incomingNonDelayedEdges

    def parent = _parent
    def setParent(n: Node) {
      _parent = n
    }

    def logic = _logic
    def setLogic(l: Logic) {
      _logic = Some(l)
    }
    
    def enabledNow = _enabledNow
    def enabledNext = _enabledNext

    def forceEnable() {
      _enabledNow = true
    }

    def forceDisable() {
      _enabledNow = false
    }

    def setNextValue(v: Boolean) {
      _enabledNext = v
    }

    private def propagateActivation() {
      for (e <- delayedEdges) {
        e.dest.activateNext()
      }
      for (e <- nonDelayedEdges) {
        e.dest.activateNow()
      }
    }

    def activateNext() {
      _enabledNext = true
      propagateActivation()
    }

    def activateNow() {
      _enabledNow = true
      _enabledNext = true
      assert(E.isEmpty)
    }

    def updateValue() {
      _enabledNow = _enabledNext
      _enabledNext = false
    }

    def connectDelayed(p: Port, r: EdgeRole) {
      val e = Edge(this, p, r)
      _delayedEdges += e
      p._incomingDelayedEdges += e
    }

    def connectNonDelayed(p: Port, r: EdgeRole) {
      val e = Edge(this, p, r)
      _nonDelayedEdges += e
      p._incomingNonDelayedEdges += e
    }

    def -->(p: Port) {
      connectDelayed(p, Activating)
    }

    def ==>(p: Port) {
      connectNonDelayed(p, Activating)
    }

    def --|(p: Port) {
      connectDelayed(p, Inhibiting)
    }

    def ==|(p: Port) {
      connectNonDelayed(p, Inhibiting)
    }

    override def toString = parent.toString + "_" + name
  }

  sealed trait EdgeRole
  case object Activating extends EdgeRole
  case object Inhibiting extends EdgeRole

  case class Edge(source: Port, dest: Port, role: EdgeRole) {
    override def toString = {
      val arrowString = role match {
        case Activating => " -> "
        case Inhibiting => " -| "
      }
      source.name + arrowString + dest.name
    }
  }

  object CellPrinter {
    def apply(c: Cell): String = {
      "cell " + c.name + " {" + "\n" +
      indent(
        (for (n <- c.N) yield apply(n)).mkString("\n\n")
      ) + "\n" +
      "}"
    }

    def apply(n: Node): String = {
      "node " + n.name + " {" + "\n" +
      indent(
        (for (l <- n.logics) yield l.toString).mkString("\n\n")
      ) + "\n" +
      "}"
    }
  }

  object CellDotPrinter {
    def apply(c: Cell): Seq[String] = {
      (for (n <- c.N) yield apply(n)).toList.flatten
    }

    def apply(n: Node): Seq[String] = {
      // println(n.name)
      (for (l <- n.logics) yield {
        l.toDotString
      }).toList
    }
  }
}
