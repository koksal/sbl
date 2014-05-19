package elegans

import Cells._
import Constraints._
import z3.scala._
import z3.scala.dsl._

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

object StatefulSemantics extends Semantics {
  type InputValue     = (Seq[Int], Seq[Int])
  type InternalState  = Int
  type ExternalState  = Int
  
  case class OutputState(name: String)

  val statefulSolutionKey = "statefulSolution"

  // use transition variables for each point p := (x, y, z) and internal state
  // s.  transition variables are bounded within 0 and number of internal
  // states
  val transitions = MutableMap[String, MutableMap[(InputValue, InternalState), Z3AST]]()

  // use mapping variables to decide which external state an internal one maps
  // to.
  val stateMappings = MutableMap[String, MutableMap[InternalState, Z3AST]]()

  val initialStates = MutableMap[String, Z3AST]()

  var solverSolution: Option[StatefulLogicSolution] = None

  /** Perform initial work with cells */
  def initializeConstraints(cells: List[Cell], scheduleLength: Int): Unit = {
    for (c <- cells) {
      for (n <- c.N) {
        n.logics collect {
          case sl: StatefulLogic => 
            sl.finalizeInternalStateSize()
            sl.populateTransitions()
            sl.populateStateMapping()
            sl.populateInitialState()
            sl.assertSolutionOptionally()
        }
      }
    }
  }

  /** Finalize constraints before solver invocation */
  def finalizeConstraints(): Unit = {
    // nothing to do
  }

  /** Reset internal state in semantics */
  def restart(): Unit = {
    transitions.clear()
    stateMappings.clear()
    initialStates.clear()
    solverSolution = None
  }

  /** Extract synthesis solution from model */
  def solution(model: Z3Model): Solution = {
    val concreteTransitions = for ((id, table) <- transitions) yield {
      val concreteTable = for ((entry, symbol) <- table) yield {
        model.evalAs[Int](symbol) match {
          case Some(modelValue) => (entry, modelValue)
          case None => terminate("No value defined for transition: " + entry)
        }
      }

      (id, concreteTable)
    }

    val concreteMappings = for ((id, mapping) <- stateMappings) yield {
      val concreteMapping = for ((internalState, externalState) <- mapping) yield {
        model.evalAs[Int](externalState) match {
          case Some(modelValue) => (internalState, modelValue)
          case None => terminate("No value defined for state mapping: " + internalState)
        }
      }

      (id, concreteMapping)
    }

    val concreteInitialStates = for ((id, initialState) <- initialStates) yield {
      model.evalAs[Int](initialState) match {
        case Some(modelValue) => (id, modelValue)
        case None => terminate("No value defined for initial state: " + id)
      }
    }

    Map(statefulSolutionKey -> StatefulLogicSolution(
      concreteTransitions, 
      concreteMappings, 
      concreteInitialStates))
  }

  /** Given the global solution instance, concretize logic instances 
   * using the given solution */
  def concretize(cells: Seq[Cell], solution: Solution): Unit = {
    solverSolution = solution.get(statefulSolutionKey) collect {
      case ss: StatefulLogicSolution => ss
    }
  }

  def assertCons(c: Z3AST) = {
    // println("ASSERTING")
    // println(c)
    ctx.assertCnstr(c)
  }

  def assertCons(c: Tree[BoolSort]) = {
    // println("ASSERTING")
    // println(c)
    ctx.assertCnstr(c)
  }

  def bundleSize(input: PortBundle): Int = input.ports.size + 1

  def allInputValues(input: PortBundle): Seq[Int] = {
    (0 to input.ports.size).toSeq
  }

  // populate the table with symbolic internal state concentration levels
  def signalValueCombinations(signals: Seq[PortBundle]): Seq[Seq[Int]] = signals match {
    case Seq() => Seq(Seq())
    case Seq(signal) => allInputValues(signal) map (value => Seq(value))
    case Seq(signal, rest @ _*) =>
      val restCombinations = signalValueCombinations(rest)
      val withNewValues = restCombinations map (combination =>
        allInputValues(signal) map (value => value +: combination)
      )
      withNewValues.flatten
  }

}

case class StatefulLogic() extends Logic {
  import StatefulSemantics._
  
  private var outputStates_ : Seq[OutputState] = Seq()
  private var initState: Option[OutputState] = None
  
  private var activatingSignals_ : Seq[PortBundle] = Seq()
  private var inhibitingSignals_ : Seq[PortBundle] = Seq()

  private var outputSignal: Option[PortBundle] = None
  private var nbInternalStates_ = -1

  def outputStates = outputStates_

  def activatingSignals = activatingSignals_
  def inhibitingSignals = inhibitingSignals_

  def nbInternalStates = nbInternalStates_

  // variables that encode the state at each timestep
  private val _stateVars = MutableMap[Int, Z3AST]()
  private def stateVar(t: Int): Z3AST = _stateVars.get(t) match {
    case Some(ast) => ast
    case None => 
      val fresh = ctx.mkFreshIntConst(this.id + "_state_" + t)
      _stateVars(t) = fresh
      fresh
  }

  def assertInitialOutputValues(): Unit = {
    // assert locally that the init state is the same as the global one
    val equality = ctx.mkEq(initialStates(id), stateVar(0))
    assertCons(equality)

    // assert local equivalence between external state and output ports
    assertOutput(stateVar(0), 0)
  }

  private def transitionInputSpace(tableSize: Int): Seq[(InputValue, InternalState)] = {
    // compute all possible activating and inhibiting signal value combinations
    val activating = signalValueCombinations(activatingSignals_)
    val inhibiting = signalValueCombinations(inhibitingSignals_)
    val inputValues = for (av <- activating; iv <- inhibiting) yield (av, iv)

    // cross them with all possible source states
    for (iv <- inputValues; stateIdx <- 0 until tableSize) yield (iv, stateIdx)
  }

  private def transitionString(entry: (InputValue, InternalState)): String = entry match {
    case ((as, is), idx) => as.mkString("_") + ":" + is.mkString("_") + ":" + idx.toString
  }

  private def weakerThan(iv1: InputValue, iv2: InputValue): Boolean = (iv1, iv2) match {
    case ((av1, iv1), (av2, iv2)) => {
      // iv1 weaker if its activating inputs are weaker and inhibiting inputs are stronger
      val activatingWeaker = (av1 zip av2).forall{
        case (v1, v2) => v1 <= v2
      }

      val inhibitingStronger = (iv1 zip iv2).forall{
        case (v1, v2) => v1 >= v2
      }

      activatingWeaker && inhibitingStronger
    }
  }

  def finalizeInternalStateSize(): Unit = {
    if (nbInternalStates_ == -1)
      nbInternalStates_ = outputStates_.size
  }

  private def assertSolution(
    ts: MutableMap[(InputValue, InternalState), InternalState],
    ms: MutableMap[InternalState, ExternalState],
    is: InternalState): Unit = {
    
    // assert transitions
    for ((k, v) <- ts) {
      val symbol = transitions(id)(k)
      val c = ctx.mkEq(symbol, ctx.mkInt(v, ctx.mkIntSort))
      assertCons(c)
    }

    // assert state mappings
    for ((k, v) <- ms) {
      val symbol = stateMappings(id)(k)
      val c = ctx.mkEq(symbol, ctx.mkInt(v, ctx.mkIntSort))
      assertCons(c)
    }

    // assert initial state
    val initEq = ctx.mkEq(initialStates(id), ctx.mkInt(is, ctx.mkIntSort))
    assertCons(initEq)
  }

  def assertSolutionOptionally(): Unit = implementation match {
    // assert manually provided solution if it exists
    case Some((ts, ms, is)) => {
      assertSolution(ts, ms, is)
    }
    case None => solverSolution match {
      case None => // no solution to assert
      case Some(StatefulLogicSolution(ts, ms, is)) => (ts.get(id), ms.get(id), is.get(id)) match {
        case (Some(ts), Some(ms), Some(is)) => assertSolution(ts, ms, is)
        case _ => logWarning("No solution found for logic: " + id)
      }
    }
  }

  def populateInitialState(): Unit = initialStates.get(id) match {
    case None => {
      val initStateSymbol = ctx.mkFreshIntConst(id + "_init")
      val upperBound = ctx.mkGE(initStateSymbol, ctx.mkInt(0, ctx.mkIntSort))
      val lowerBound = ctx.mkLT(initStateSymbol, ctx.mkInt(nbInternalStates_, ctx.mkIntSort))
      assertCons(upperBound)
      assertCons(lowerBound)

      initialStates(id) = initStateSymbol

      // assert globally that the init state maps to the specified external state
      val mapping = stateMappings(id)
      for ((internalStateValue, externalStateSymbol) <- mapping) {
        val implication = ctx.mkImplies(
          // if internal init state is "internalStateValue"
          ctx.mkEq(initialStates(id), ctx.mkInt(internalStateValue, ctx.mkIntSort)),
          // then its mapping "externalStateSymbol" should be the user-provided
          // init state
          ctx.mkEq(externalStateSymbol, ctx.mkInt(initStateIndex(), ctx.mkIntSort))
        )
        assertCons(implication)
      }

    }
    case Some(_) => // do nothing if initial state is already populated
  }

  def populateStateMapping(): Unit = stateMappings.get(id) match {
    case None => {
      val toPopulate = MutableMap[InternalState, Z3AST]()

      for (i <- 0 until nbInternalStates_) {
        val fresh = ctx.mkFreshIntConst(id + "_mapping_" + i)
        assertCons(ctx.mkGE(fresh, ctx.mkInt(0, ctx.mkIntSort)))
        assertCons(ctx.mkLT(fresh, ctx.mkInt(outputStates_.size, ctx.mkIntSort)))
        toPopulate(i) = fresh
      }

      // internal boundary states map to external boundary states
      val firstStateCons  = ctx.mkEq(toPopulate(0), ctx.mkInt(0, ctx.mkIntSort))
      val lastStateCons   = ctx.mkEq(
        toPopulate(nbInternalStates_ - 1), 
        ctx.mkInt(outputStates_.size - 1, ctx.mkIntSort))

      assertCons(firstStateCons)
      assertCons(lastStateCons)

      // mapping is monotonic
      for (i <- 1 until nbInternalStates_) {
        val lessThan = ctx.mkLE(toPopulate(i-1), toPopulate(i))
        assertCons(lessThan)
      }

      stateMappings(id) = toPopulate

    }
    case Some(_) => // do nothing if mapping is already populated
  }

  def populateTransitions(): Unit = transitions.get(id) match {
    case None => {
      val toPopulate = MutableMap[(InputValue, InternalState), Z3AST]()

      for (entry <- transitionInputSpace(nbInternalStates_)) {
        val fresh = ctx.mkFreshIntConst(transitionString(entry))
        assertCons(ctx.mkGE(fresh, ctx.mkInt(0, ctx.mkIntSort)))
        assertCons(ctx.mkLT(fresh, ctx.mkInt(nbInternalStates_, ctx.mkIntSort)))
        toPopulate(entry) = fresh
      }

      // assert that if (x, y, z) <= (x', y', z') then f(s, p) <= f(s, p')
      // where s is an internal state of this logic
      for (((inputVal1, state1), symbol1) <- toPopulate; 
           ((inputVal2, state2), symbol2) <- toPopulate;
           if (weakerThan(inputVal1, inputVal2) && state1 == state2)) {
        // TODO only assert for neighboring points
        assertCons(ctx.mkLE(symbol1, symbol2))
      }
      
      // assert that if s < s', then f(s, p) <= f(s', p)
      for (((inputVal1, state1), symbol1) <- toPopulate; 
           ((inputVal2, state2), symbol2) <- toPopulate;
          if (inputVal1 == inputVal2 && state1 <= state2)) {
        // TODO only assert for neighboring states
        assertCons(ctx.mkLE(symbol1, symbol2))
      }

      // println("POPULATED")
      // println(toPopulate.mkString("\n"))
      // println
      transitions(id) = toPopulate

    }
    case Some(_) => 
      // do nothing if transitions were already populated
  }

  def state(name: String): OutputState = {
    val newState = OutputState(name)
    outputStates_ = outputStates_ :+ newState
    newState
  }

  def activating(bundle: PortBundle) {
    activatingSignals_ = activatingSignals_ :+ bundle
  }
  
  def inhibiting(bundle: PortBundle) {
    inhibitingSignals_ = inhibitingSignals_ :+ bundle
  }

  /** Relate output ports to output states of the logic */
  def output(bundle: PortBundle) {
    outputSignal = Some(bundle)
  }
  
  def init(s: OutputState) {
    initState = Some(s)
  }

  def nbStates(n: Int) {
    nbInternalStates_ = n
  }

  /** Returns a symbolic integer that is constrained to the input bundle value
   * at time t */
  private def bundle2intVar(input: PortBundle, t: Int): Z3AST = {
    val name = input.id + "_" + t.toString
    val fresh = ctx.mkFreshIntConst(name)

    assertCons(relaxedBooleanIntEquivalence(input, t, fresh))

    fresh
  }

  /** Asserts that the output bundle at time t is equal to the value of the
   * given state variable */
  private def assertOutput(stateVariable: Z3AST, t: Int): Unit = outputSignal match {
    case None => terminate("Output ports not specified for logic: " + id)
    case Some(signal) => stateMappings.get(id) match {
      case Some(mapping) => {
        // unroll internal to external mapping
        for ((internalState, externalState) <- mapping) {
          val implication = ctx.mkImplies(
            ctx.mkEq(stateVariable, ctx.mkInt(internalState, ctx.mkIntSort)),
            booleanIntEquivalence(signal, t, externalState)
          )
          assertCons(implication)
        }
      }
      case None => throw new Exception("State mapping not specified for logic: " + id)
    }
  }

  private def booleanIntEquivalence(bundle: PortBundle, t: Int, intVar: Z3AST): Z3AST = {
    val is = ctx.mkIntSort
    val bundlePorts = bundle.ports

    // if all ports are false then integer value is 0
    val allFalse = ctx.mkAnd(bundlePorts.map(port => ctx.mkNot(portVars(t)(port))): _*)
    val zeroCond = ctx.mkIff(allFalse, ctx.mkEq(intVar, ctx.mkInt(0, is)))

    // if any port is activated, the integer value is the 1-based index
    val otherConds = for ((p, idx) <- bundlePorts zipWithIndex) yield {
      ctx.mkIff(portVars(t)(p), ctx.mkEq(intVar, ctx.mkInt(idx + 1, is)))
    }

    // for sanity, add bound constraints
    val bounds = ctx.mkAnd(
      ctx.mkGE(intVar, ctx.mkInt(0, is)),
      ctx.mkLE(intVar, ctx.mkInt(bundlePorts.size, is))
    )

    val toConjunct = Seq(zeroCond, bounds) ++ otherConds
    ctx.mkAnd(toConjunct: _*)
  }

  private def relaxedBooleanIntEquivalence(bundle: PortBundle, t: Int, intVar: Z3AST): Z3AST = {
    val is = ctx.mkIntSort
    val bundlePorts = bundle.ports

    // if all ports are false then integer value is 0
    val allFalse = ctx.mkAnd(bundlePorts.map(port => ctx.mkNot(portVars(t)(port))): _*)
    val zeroCond = ctx.mkIff(allFalse, ctx.mkEq(intVar, ctx.mkInt(0, is)))

    // if any port is activated and the greater ones are disabled, the integer
    // value is the 1-based index
    val otherConds = for ((p, idx) <- bundlePorts zipWithIndex) yield {
      val thisPortEnabled = portVars(t)(p)
      val nextPortsDisabled = bundlePorts.drop(idx + 1).map(other => ctx.mkNot(portVars(t)(other)))
      val portCond = ctx.mkAnd((thisPortEnabled +: nextPortsDisabled): _*)
      ctx.mkIff(portCond, ctx.mkEq(intVar, ctx.mkInt(idx + 1, is)))
    }

    val toConjunct = zeroCond +: otherConds
    ctx.mkAnd(toConjunct: _*)
  }

  private def seqLT(s1: Seq[Int], s2: Seq[Int]): Boolean = (s1, s2) match {
    case (Seq(e1, r1 @ _*), Seq(e2, r2 @ _*)) =>
      e1 < e2 || (
        e1 == e2 && (
          seqLT(r1, r2)
        )
      )
    case (Seq(), Seq()) => false
  }

  private def transitionLT(
    r1: ((InputValue, InternalState), InternalState),
    r2: ((InputValue, InternalState), InternalState)
  ): Boolean = (r1, r2) match {
    case ((((a1, i1), s1), d1), (((a2, i2), s2), d2)) =>
      s1 < s2 || (
        s1 == s2 &&
        (
          seqLT(a1, a2) || (
            a1 == a2 && seqLT(i1, i2)
          )
        )
        
      )
  }

  override def toDotString: String = {
    solverSolution match {
      case Some(StatefulLogicSolution(ct, cm, cis)) => (ct.get(id), cm.get(id), cis.get(id)) match {
        case (Some(logicTable), Some(mapping), Some(initState)) => {
          //stupid craps...
          def mkInit() : String = {"INIT"}
          //looks like this: internal[external]
          def mkIntern(x : Int) : String = {
            "int_"+x+"["+mapping(x)+"]"
          }
          def quote(x : String) : String = {"\""+x+"\""}
          def conn(x : String, y : String) = {
            quote(x) + " -> " + quote(y)
          }
          def newl(x : String) : String = {x+";\n"}

          def connectInit() = {
            newl(conn(mkInit(), mkIntern(initState)))
          }
          def drawClusters() = {
            //this table is a cluster f**k, basically another representation of logicTable
            //from internal level to
            //a list of stuff that lives in it (all concentrations)
            //each concentration again points to another internal level
            //yeah...
            var clusterTable : MutableMap[Int,List[((Seq[Int],Seq[Int]),Int)]] = MutableMap[Int,List[((Seq[Int],Seq[Int]),Int)]]() 
            //populate the clusterTable...
            for ((((actVals, inhVals), src), dst) <- logicTable) {
              clusterTable += src -> List()
            }
            for ((((actVals, inhVals), src), dst) <- logicTable) {
              clusterTable += (src -> 
                                (clusterTable(src) ++ List(((actVals,inhVals),dst)))
                              )
            }
            val sortedTable = clusterTable.toList.sortBy(_._1)
            //use the table to draw the clusters
            //must be done in 2 parts, first draw the cluster
            //then connect the nodes around with edges
            def drawCluster(level : Int, concs : List[((Seq[Int],Seq[Int]),Int)]) = {
              def incomming(clusNum: Int) : String = "\"lvl_"+clusNum+"_in\""
              def outgoing(clusNum: Int, dst: Int) : String = "\"lvl_"+clusNum+"_to_"+dst+"\""
              var clustr = "subgraph cluster_lvl_"+level+"{\n"+
                    "label=\"lvl_"+level+"\";\n"+
                    "style=\"rounded\";\n"
              //each cluster has 1 input and multiple outputs
              for (otherNode <- sortedTable.map(_._1).sorted){
                clustr += outgoing(level, otherNode)+";\n"
              }
              for (((acts, inhs), dst) <- concs.sortBy(_._2).reverse){
                clustr += "\"#"+level+"(" + acts.mkString("[",",","]") + "," +
                                inhs.mkString("[",",","]") + ")\"" +
                          " -> " + outgoing(level, dst) + ";\n"
              }
              clustr += "}\n"
              clustr
            }
            var ret = ""
            for ((clust, concs) <- sortedTable) {
              ret += drawCluster(clust, concs)
            }
            ret
          }
          def connectInterns() = {
            def collapseTable() = {
              var table : MutableMap[(Int,Int),String] = MutableMap[(Int,Int),String]()
              for ((((actVals, inhVals), src), dst) <- logicTable) {
                table += (src, dst) -> ""
              }
              def withName(acts : Seq[Int], inhs : Seq[Int]) : String = {
                val activatingStrings = (acts zip activatingSignals_) map {
                  case (value, bundle) => bundle.ports.map(_.name).mkString("_") + " = " + value
                }
                val inhibitingStrings = (inhs zip inhibitingSignals_) map {
                  case (value, bundle) => bundle.ports.map(_.name).mkString("_") + " = " + value
              }
                activatingStrings.mkString("") ++ inhibitingStrings.mkString("")
              }
              for ((((actVals, inhVals), src), dst) <- logicTable) {
                table += (src, dst) -> (table((src,dst)) + withName(actVals, inhVals)) 
              }
              table
            }
            collapseTable().toString()
          }
          def initAndLegend() : String = {
            "\"("+activatingSignals_.map(_.ports).flatten.mkString("[",",","]")+","+
                 inhibitingSignals_.map(_.ports).flatten.mkString("[",",","]")+")\";\n"
          }
          def getEdges() : String = {
            connectInit() +
            connectInterns()
          }
          "digraph G {\n" + initAndLegend() + drawClusters() + "}\n"
        }
        case _ => "??"
      }
      case _ => "??"
    }
  }
  override def toString: String = {
    solverSolution match {
      case Some(StatefulLogicSolution(ct, cm, cis)) => (ct.get(id), cm.get(id), cis.get(id)) match {
        case (Some(logicTable), Some(mapping), Some(initState)) => {
          val sortedTable = logicTable.toList.sortWith(transitionLT)
          val rowStrings = sortedTable map {
            case (((actValues, inhValues), srcState), destState) =>
              val activatingStrings = (actValues zip activatingSignals_) map {
                case (value, bundle) => bundle.ports.map(_.name).mkString("_") + " = " + value
              }
              val inhibitingStrings = (inhValues zip inhibitingSignals_) map {
                case (value, bundle) => bundle.ports.map(_.name).mkString("_") + " = " + value
              }
              "(src: " + srcState + ",in: " +
              (activatingStrings ++ inhibitingStrings).mkString(";") + ")" +
              " => " + destState + " (" + outputStates_(mapping(destState)).name + ")"
          }

          "init: " + initState + "\n" +
          rowStrings.mkString("\n")
        }
        case _ => "??"
      }
      case _ => "??"
    }
  }

  /** Assert constraints for deciding output values associated with this logic
   * */
  def assertLogic(t: Int): Unit = {
    // make an int variable that represents each input's strength at this time
    val activatingIntVars = activatingSignals_ map (bundle2intVar(_, t))
    val inhibitingIntVars = inhibitingSignals_ map (bundle2intVar(_, t))
    
    // we are then going to unroll for each value of inputs and for each value
    // of the state the definition of the transition
    val inputsAreCovered = scala.collection.mutable.Set[Z3AST]()
    transitions.get(id) match {
      case Some(table) => {
        for ((key, value) <- table) {
          val (cnstr, inputCond) = transitionConstraint(activatingIntVars, inhibitingIntVars, key, value, t)
          inputsAreCovered += inputCond
          assertCons(cnstr)
        }
      }
      case None => logError("No transition table for logic: " + this.id)
    }
    
    // don't forget to assert value of output variable
    assertOutput(stateVar(t), t)
  }

  private def onlyOneTrue(asts: Seq[Z3AST]) = {
    val allPossibleInputSpace = for (astTrue <- asts) yield {
      val astFalses = asts filter (_ != astTrue)
      val everyoneFalse = astFalses map (ctx.mkNot(_))
      val iAmTheOnlyTrue = ctx.mkAnd(astTrue, ctx.mkAnd(everyoneFalse: _*))
      iAmTheOnlyTrue
    }
    ctx.assertCnstr(ctx.mkOr(allPossibleInputSpace: _*))
  }

  /** Constructs the constraint for deciding value of state at time t if the
   * given transition edge is taken at t-1 */
  private def transitionConstraint(
      activatingVars: Seq[Z3AST],
      inhibitingVars: Seq[Z3AST],
      entry: (InputValue, InternalState),
      destination: Z3AST,
      t: Int): (Z3AST, Z3AST) = {
    val signalValues = entry._1
    val stateValue = entry._2
    val (activatingValues, inhibitingValues) = signalValues
    val activatingEqual = (activatingVars zip activatingValues) map {
      case (variable, value) => ctx.mkEq(variable, ctx.mkInt(value, ctx.mkIntSort))
    }
    val inhibitingEqual = (inhibitingVars zip inhibitingValues) map {
      case (variable, value) => ctx.mkEq(variable, ctx.mkInt(value, ctx.mkIntSort))
    }
    val stateEquals = ctx.mkEq(stateVar(t - 1), ctx.mkInt(stateValue, ctx.mkIntSort))
    val conjunctSeq = activatingEqual ++ inhibitingEqual ++ Seq(stateEquals)
    val indexingMatches = ctx.mkAnd(conjunctSeq: _*)
    (ctx.mkImplies(indexingMatches, ctx.mkEq(stateVar(t), destination)), indexingMatches)
  }

  var initialized: Boolean = false
  var currentState: InternalState = -1

  def initStateIndex(): ExternalState = initState match {
    case None => terminate("init state not defined for: " + id)
    case Some(is) => outputStates_.indexOf(is)
  }

  /** Take one step in concrete evaluation */
  def act(): Unit = {
    val (ct, cm, is) = implementation match {
      case Some((ct, cm, is)) => (ct, cm, is)
      case None => solverSolution match {
        case Some(StatefulLogicSolution(ct, cm, is)) => (ct(id), cm(id), is(id))
        case _ => throw new Exception("unconcretized node")
      }
    }
      
    if (!initialized) {
      currentState = is
      assert(currentState >= 0)
      initialized = true
    }
    val activatingInputValue = activatingSignals_ map bundle2int
    val inhibitingInputValue = inhibitingSignals_ map bundle2int
    val inputValue = ((activatingInputValue, inhibitingInputValue), currentState)
    val nextState = ct(inputValue)
    val nextExternalState = cm(nextState)
    currentState = nextState
    setNextBundleValue(outputSignal.get, nextExternalState)
  }

  private def bundle2int(bundle: PortBundle): Int = {
    var toRet = 0
    for ((port, index) <- bundle.ports zipWithIndex) {
      if (port.enabledNext)
        toRet = index + 1
    }
    toRet
  }

  def setNextBundleValue(bundle: PortBundle, nextValue: Int) {
    if (nextValue > 0)
      bundle.ports(nextValue - 1).setNextValue(true)
  }

  private var implementation: Option[(
    MutableMap[(InputValue, InternalState), InternalState],
    MutableMap[InternalState, ExternalState],
    InternalState
    )] = None

  def setImplementation(
    tr: Map[(InputValue, InternalState), InternalState],
    sm: Map[InternalState, ExternalState],
    is: InternalState
  ) {
    setImplementation(
      MutableMap() ++ tr,
      MutableMap() ++ sm,
      is
    )
  }

  def setImplementation(
    tr: MutableMap[(InputValue, InternalState), InternalState],
    sm: MutableMap[InternalState, ExternalState],
    is: InternalState
  ) {
    implementation = Some((tr, sm, is))
  }
}

import StatefulSemantics._

case class StatefulLogicSolution(
  transitions: MutableMap[String, MutableMap[(InputValue, InternalState), InternalState]],
  stateMappings: MutableMap[String, MutableMap[InternalState, ExternalState]],
  initialInternalState: MutableMap[String, InternalState]
) extends LogicSolution

