package elegans

import StatefulSemantics._
import Cells._
import Experiments._

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

import z3.scala._

object Summaries {

  type EdgeSet = MutableMap[(InputValue, InternalState), InternalState]

  case class Region(lowerCorner: Seq[Option[Int]], upperCorner: Seq[Option[Int]])

  def summarize(solution: Option[Constraints.Solution], 
      experiments: Seq[Experiment], modelIndex: Int): Unit = {
    val (nonvpcs, aPrioriChannels, vpcs) = Model.createSystem(experiments.head)

    val allCells = nonvpcs ::: vpcs

    solution match {
      case Some(sol) => 
        concretize(allCells, sol)
      case None =>
    }
    
    solution match {
      case None => logWarning("No solution defined.")
      case Some(sol) => sol.get(statefulSolutionKey) match {
        case Some(statefulSol: StatefulLogicSolution) =>
          for (asyncCell <- vpcs) {
            for (node <- asyncCell.N) {
              for (sl @ StatefulLogic() <- node.logics) {
                summarize(sl, statefulSol, modelIndex)
              }
            }
          }
        case _ => logWarning("No stateful solution defined.")
      }
    }
  }

  def summarize(logic: StatefulLogic, solution: StatefulLogicSolution,
      modelIndex: Int): Unit = {
    val allSignals = logic.activatingSignals ++ logic.inhibitingSignals

    def simplifyTransitions(transitions: EdgeSet) = {
      transitions
    }

    def usedSrcStates(transitions: EdgeSet) = {
      val srcStates = transitions.map{
        case ((v, src), dst) => src
      }.toSet

      srcStates
    }

    def edgeSummaryString(edges: Set[Region]): String = {
      val strs = edges.map{
        case Region(l, u) =>
          val edgeStr = ((l zip u) zip allSignals) collect {
            case ((Some(lv), Some(uv)), bundle) =>
              if (lv == uv) {
                bundle.id + " = " + lv
              } else {
                bundle.id + " in [" + lv + ":" + uv + "]"
              }
            case ((Some(_), None), _) => terminate("Should not have happened.")
            case ((None, Some(_)), _) => terminate("Should not have happened.")
          }
          edgeStr.mkString(", ")
      }
      strs.mkString("\\n")
    }

    def simplifyCovering(covering: Set[Region]): Set[Region] = {
      covering map {
        case Region(lb, ub) =>
          val simplifiedBounds = ((lb zip ub) zip allSignals) map {
            case ((Some(lp), Some(up)), bundle) =>
              // if they cover the range, drop them
              if (lp == 0 && up == bundleSize(bundle) - 1)
                (None, None)
              else
                (Some(lp), Some(up))
            case _ => terminate("Should not have happened.")
          }
          val (simplifiedLb, simplifiedUb) = simplifiedBounds.unzip
          Region(simplifiedLb, simplifiedUb)
      }
    }

    def findMinimalCovering(includedEdges: EdgeSet, complementEdges: EdgeSet) = {
      val dimension: Int = {
        val e = includedEdges.head
        e._1._1._1.size + e._1._1._2.size
      }

      def findCovering(nbRegions: Int): Option[Set[Region]] = {
        val ctx = new Z3Context("MODEL" -> true)

        val regionVars = for (i <- 0 until nbRegions) yield {
          val lower = for (j <- 0 until dimension) yield {
            ctx.mkFreshIntConst("region_" + i + "_lower_" + j)
          }

          val upper = for (j <- 0 until dimension) yield {
            ctx.mkFreshIntConst("region_" + i + "_upper_" + j)
          }

          (lower.toList, upper.toList)
        }

        // assert bounds on "corners"

        for ((lowerCorner, upperCorner) <- regionVars) {
          (lowerCorner zip allSignals) foreach {
            case (point, bundle) =>
              ctx.assertCnstr(ctx.mkGE(point, ctx.mkInt(0, ctx.mkIntSort)))
              ctx.assertCnstr(ctx.mkLT(point, ctx.mkInt(bundleSize(bundle), ctx.mkIntSort)))
          }
        
          (upperCorner zip allSignals) foreach {
            case (point, bundle) =>
              ctx.assertCnstr(ctx.mkGE(point, ctx.mkInt(0, ctx.mkIntSort)))
              ctx.assertCnstr(ctx.mkLT(point, ctx.mkInt(bundleSize(bundle), ctx.mkIntSort)))
          }

          (lowerCorner zip upperCorner) foreach {
            case (p1, p2) =>
              ctx.assertCnstr(ctx.mkLE(p1, p2))
          }
        }

        // assert all edges are in a rectangle
        for (edge <- includedEdges) {
          val (actInputValue, inhInputValue) = edge._1._1
          val inputValues = actInputValue ++ inhInputValue
          // some rectangle includes this input value
          val includedInRectangles = for ((lowerCorner, upperCorner) <- regionVars) yield {
            val lowerBounds = (lowerCorner zip inputValues) map {
              case (point, value) =>
                ctx.mkLE(point, ctx.mkInt(value, ctx.mkIntSort))
            }
            val upperBounds = (upperCorner zip inputValues) map {
              case (point, value) =>
                ctx.mkGE(point, ctx.mkInt(value, ctx.mkIntSort))
            }

            ctx.mkAnd(ctx.mkAnd(lowerBounds: _*), ctx.mkAnd(upperBounds: _*))
          }

          ctx.assertCnstr(ctx.mkOr(includedInRectangles: _*))
        }

        // assert all excluded edges are not in any rectangle
        for (edge <- complementEdges) {
          val (actInputValue, inhInputValue) = edge._1._1
          val inputValues = actInputValue ++ inhInputValue

          // no rectangle includes this input value
          val notIncludedInRectangles = for ((lowerCorner, upperCorner) <- regionVars) yield {
            val lowerBounds = (lowerCorner zip inputValues) map {
              case (point, value) =>
                ctx.mkLE(point, ctx.mkInt(value, ctx.mkIntSort))
            }
            val upperBounds = (upperCorner zip inputValues) map {
              case (point, value) =>
                ctx.mkGE(point, ctx.mkInt(value, ctx.mkIntSort))
            }

            ctx.mkNot(
              ctx.mkAnd(ctx.mkAnd(lowerBounds: _*), ctx.mkAnd(upperBounds: _*))
            )
          }

          ctx.assertCnstr(ctx.mkAnd(notIncludedInRectangles: _*))
        }
  
        ctx.checkAndGetModel match {
          case (Some(true), m) =>
            val regions = for ((lowerCorner, upperCorner) <- regionVars) yield {
              val lowerCornerValues = lowerCorner.map(point => m.evalAs[Int](point))
              val upperCornerValues = upperCorner.map(point => m.evalAs[Int](point))
              Region(lowerCornerValues, upperCornerValues)
            }
            m.delete
            ctx.delete
            Some(regions.toSet)
          case _ => None
        }

      }
    
      var coveringNotFound = true
      var coveringSolution: Option[Set[Region]] = None

      for (nbRegions <- 1 to includedEdges.size; if coveringNotFound) {
        findCovering(nbRegions) match {
          case Some(cs) =>
            coveringNotFound = false
            coveringSolution = Some(cs)
          case None =>
        }
      }

      coveringSolution match {
        case None => terminate("No covering found!")
        case Some(cs) => cs
      }
    }

    type TransitionRegionMap = MutableMap[(InternalState, InternalState), Set[Region]]

    def usedStates(ts: TransitionRegionMap): Set[InternalState] = {
      val (srcStates, dstStates) = ts map {
        case ((src, dst), rs) => (src, dst)
      } unzip

      srcStates.toSet ++ dstStates.toSet
    }

    def incomingEdges(ts: TransitionRegionMap, dst: InternalState) = {
      ts.filter {
        case ((_, d), _) => d == dst
      }
    }

    // trees for flow analysis
    sealed trait Formula
    sealed trait Term
    case class Variable(id: String) extends Term
    case class IntegerValue(value: Int) extends Term
    case object False extends Formula
    case object True extends Formula
    case class Equals(t1: Term, t2: Term) extends Formula
    case class LessThanEquals(t1: Term, t2: Term) extends Formula
    case class GreaterThanEquals(t1: Term, t2: Term) extends Formula
    case class And(f1: Formula, f2: Formula) extends Formula
    case class Or(f1: Formula, f2: Formula) extends Formula

    def simplifyWithFlowAnalysis(
        transitionsAsRegions: MutableMap[(InternalState, InternalState), Set[Region]],
        initState: InternalState): MutableMap[(InternalState, InternalState), Set[Region]] = {

      val allStates = usedStates(transitionsAsRegions)

      val ctx = new Z3Context()

      // create variables for each region and for each analysis step.
      val z3Vars = MutableMap[(PortBundle, InternalState, Int), Z3AST]()

      // declare the formulas that describe the new allowed values for each state at each time step
      val stateFormulas = MutableMap[(InternalState, Int), Z3AST]()

      // construct formulas that encode initial permissible values for each internal state
      //    these are namely true for the init state, false for others.
      for (state <- allStates) {
        if (state == initState)
          stateFormulas((state, 0)) = ctx.mkTrue
        else
          stateFormulas((state, 0)) = ctx.mkFalse
      }

      def createVariablesForStep(step: Int) {
        for (state <- allStates) {
          for (bundle @ PortBundle(id, ports) <- allSignals) {
            val fresh = ctx.mkFreshIntConst("state_" + state + "_" + id + "_" + step)
            val size = bundleSize(bundle)
            val lb = ctx.mkGE(fresh, ctx.mkInt(0, ctx.mkIntSort))
            val ub = ctx.mkLT(fresh, ctx.mkInt(size, ctx.mkIntSort))
            val bounds = ctx.mkAnd(lb, ub)
            ctx.assertCnstr(bounds)
            z3Vars((bundle, state, step)) = fresh
          }
        }
      }

      // construct formula that constrains vars with given regions
      def constrainWithRegions(vars: Seq[Z3AST], regions: Set[Region]): Z3AST = {
        val disjuncts = for (Region(lb, ub) <- regions) yield {
          val rowConstraints = ((lb zip ub) zip vars) collect {
            case ((Some(lv), Some(uv)), v) =>
              val lower = ctx.mkGE(v, ctx.mkInt(lv, ctx.mkIntSort))
              val upper = ctx.mkLE(v, ctx.mkInt(uv, ctx.mkIntSort))
              ctx.mkAnd(lower, upper)
          }
          if (rowConstraints.isEmpty)
            ctx.mkTrue
          else
            ctx.mkAnd(rowConstraints: _*)
        }
        if (disjuncts.isEmpty)
          ctx.mkFalse
        else
          ctx.mkOr(disjuncts.toList : _*)
      }

      def assumptionConstraints(srcVars: Seq[Z3AST], dstVars: Seq[Z3AST], assumptions: Seq[Set[Assumption]]): Z3AST = {
        val cnstrs = ((srcVars zip dstVars) zip assumptions) collect {
          case ((src, dst), as) if !as.isEmpty =>
            val cnstrsForDimension = as map {
              case Constant =>
                ctx.mkEq(src, dst)
              case Monotonic =>
                ctx.mkLE(src, dst)
            }
            ctx.mkAnd(cnstrsForDimension.toList : _*)
        }

        if (cnstrs.isEmpty)
          ctx.mkTrue
        else
          ctx.mkAnd(cnstrs: _*)
      }

      def containsPoint(state: InternalState, steps: Seq[Int], point: Seq[Int]): Boolean = {
        ctx.push

        val disjuncts = for (step <- steps) yield {
          val componentsEqualValues = (allSignals zip point) map {
            case (bundle, value) => 
              ctx.mkEq(
                z3Vars((bundle, state, step)),
                ctx.mkInt(value, ctx.mkIntSort)
              )
          }

          val constraintForStep = 
            componentsEqualValues.foldLeft(stateFormulas((state, step)))(ctx.mkAnd(_, _))

          constraintForStep
        }

        ctx.assertCnstr(disjuncts.foldLeft(ctx.mkFalse)(ctx.mkOr(_, _)))

        val outcome = ctx.check match {
          case Some(true) => true
          case _ => false
        }

        ctx.pop(1)

        outcome
      }

      def fixpointReachedForState(state: InternalState, step: Int): Boolean = {
        val allPossiblePoints = signalValueCombinations(allSignals)

        var differenceNonEmpty = false
        for (point <- allPossiblePoints; if !differenceNonEmpty) {
          // does the new space contain the point?
          if (containsPoint(state, Seq(step), point)) {
            // if yes, can we make sure the old spaces don't contain it?
            if (! containsPoint(state, (0 until step).toList, point)) {
              // if yes, then fixpoint not reached, return false
              differenceNonEmpty = true
            } else {
              // if no, then move on to the next point
            }
          }
          // if no, skip this point
        }

        !differenceNonEmpty
      }

      def regionIntersects(region: Region, formulas: MutableMap[(InternalState, Int), Z3AST]): Boolean = {
        ctx.push
        val disjuncts = formulas map {
          case ((srcState, step), formula) =>
            val varsAtThisStep = allSignals map {
              bundle => z3Vars((bundle, srcState, step))
            }
            val isWithinRegionCnstr = constrainWithRegions(varsAtThisStep, Set(region))
            ctx.mkAnd(formula, isWithinRegionCnstr)
        }
        val intersectsAtAnyTime = disjuncts.foldLeft(ctx.mkFalse)(ctx.mkOr(_, _))
        ctx.assertCnstr(intersectsAtAnyTime)
        val outcome = ctx.check match {
          case Some(true) => true
          case _ => false
        }
        ctx.pop(1)
        outcome
      }

      def pruneUnusableEdges(transitionsAsRegions: TransitionRegionMap): TransitionRegionMap = {
        val prunedMap: TransitionRegionMap = MutableMap[(InternalState, InternalState), Set[Region]]()
        for (((src, dst), regions) <- transitionsAsRegions) {
          val prunedRegions = regions.filter {
            region =>
              val formulasForSrc = stateFormulas.filter {
                case ((s, _), f) => s == src
              }
              regionIntersects(region, formulasForSrc)
          }
          if (!prunedRegions.isEmpty)
            prunedMap += (((src, dst), prunedRegions))
        }
        prunedMap
      }

      // then, loop:
      //    at each step, compute new formulas that encode one step of incoming flow for each state
      //    check whether the space for any node increases in the new step

      var currentStep = 1
      var fixpointReached = false

      createVariablesForStep(0)

      while(!fixpointReached) {
        // create variables for each state
        createVariablesForStep(currentStep)

        // compute next input for each state
        for (state <- allStates) {
          // assert that current values at dst are the same + incoming region constraints

          val constraintsForEdges = for (((src, _), regions) <- incomingEdges(transitionsAsRegions, state)) yield {
            val srcVarsAtPrevStep = allSignals map {
              bundle => z3Vars((bundle, src, currentStep - 1))
            }

            val dstVarsAtCurrentStep = allSignals map {
              bundle => z3Vars((bundle, state, currentStep))
            }

            val assumptions = allSignals map {
              bundle => bundle.assumptions
            }

            val srcCnstrAtPrevStep = stateFormulas((src, currentStep - 1))

            // we want constrain the src constraint further with the edge constraint
            val constrainedByEdge = constrainWithRegions(srcVarsAtPrevStep, regions)

            // assert assumptions about this incoming edge
            val assumptionCnstr = assumptionConstraints(srcVarsAtPrevStep, dstVarsAtCurrentStep, assumptions)

            // we conjunct all the three above to obtain what's added to the space
            val constraintForEdge = ctx.mkAnd(srcCnstrAtPrevStep, constrainedByEdge, assumptionCnstr)

            constraintForEdge
          }

          val disjunct = constraintsForEdges.foldLeft(ctx.mkFalse)(ctx.mkOr(_, _))
          stateFormulas((state, currentStep)) = ctx.mkOr(disjunct)
        }

        // is the fixpoint reached for all states?
        fixpointReached = allStates.forall(fixpointReachedForState(_, currentStep))
        currentStep += 1
      }

      // when fixpoint is reached, check whether any outgoing row can be removed from transitions
      val toRet = pruneUnusableEdges(transitionsAsRegions)

      ctx.delete

      toRet
    }

    log("Summarizing logic " + logic.id)

    solution match {
      case StatefulLogicSolution(allTransitions, allMappings, initStates) => {
        val transitions = allTransitions(logic.id)
        val mapping = allMappings(logic.id)
        val initState = initStates(logic.id)

        def fsmAsDotString(
            edgeStrings: MutableMap[(InternalState, InternalState), String]): String = {
          
          def stateName(s: InternalState) = {
            val extIndex = mapping(s)
            val extName = logic.outputStates(extIndex).name
            "S" + s + "_" + extName
          }
          def edgeBoxName(s1: InternalState, s2: InternalState) = "E_" + s1 + "_" + s2

          val res = new StringBuffer()

          res append "digraph finite_state_machine {\n"
          res append """  size="15"
  node [color = blue];"""
          res append (stateName(initState) + ";\n")
          res append "  node [color = black];\n"

          for (((src, dst), label) <- edgeStrings) {
            res append ("  " + edgeBoxName(src, dst) + " [ shape = box label = " + 
              "\"" + label + "\"" + " ];\n")
            val toEdgeBox   = "  " + stateName(src) + " -> " + edgeBoxName(src, dst) + ";\n"
            val fromEdgeBox = "  " + edgeBoxName(src, dst) + " -> " + stateName(dst) + ";\n"
            res append toEdgeBox
            res append fromEdgeBox
          }
          res append "}"

          res.toString
        }

        (allTransitions.get(logic.id), allMappings.get(logic.id), initStates.get(logic.id)) match {
          case ((Some(transitions), Some(mapping), Some(initState))) => {
            val simplifiedTransitions = simplifyTransitions(transitions)
            
            val validSrcStates = usedSrcStates(simplifiedTransitions)
            
            val simplifiedTransitionTable = MutableMap[(InternalState, InternalState), Set[Region]]()

            for (srcState <- validSrcStates) {
              val outgoingEdges = simplifiedTransitions.filter{
                case ((v, s1), s2) => s1 == srcState
              }

              val dstStatesForSrc = outgoingEdges.map{
                case ((v, s1), s2) => s2
              }.toSet

              for (dstState <- dstStatesForSrc) {
                val edgesBetweenSrcAndDest = outgoingEdges.filter{
                  case ((v, s1), s2) => s2 == dstState
                }

                val complementEdges = transitions.filter{
                  case ((v, s1), s2) => s1 == srcState
                } -- (edgesBetweenSrcAndDest.keys)

                val minimalCovering = findMinimalCovering(edgesBetweenSrcAndDest, complementEdges)
                val simplifiedCovering = simplifyCovering(minimalCovering)

                simplifiedTransitionTable((srcState, dstState)) = simplifiedCovering
              }
            }

            val simplifiedWithFlowAnalysis = simplifyWithFlowAnalysis(simplifiedTransitionTable, initState)

            def generateImages(ts: TransitionRegionMap, suffix: String) {
              val edgeStringMap = ts map {
                case ((src, dst), regions) => ((src, dst), edgeSummaryString(regions))
              }
              val fsmDotStr = fsmAsDotString(edgeStringMap)

              val dotPath = logic.id + "_" + suffix + "_" + modelIndex + ".dot"
              val imgPath = logic.id + "_" + suffix + "_" + modelIndex + ".pdf"

              writeToFile(dotPath, fsmDotStr)

              import scala.sys.process._
              ("dot -Tpdf -o " + imgPath + " " + dotPath).!!
              ("rm " + dotPath).!!
            
            }

            generateImages(simplifiedTransitionTable, "collapsed")
            generateImages(simplifiedWithFlowAnalysis, "pruned")
          }

          case _ => // do nothing
        }
      }
    }
  }
}
