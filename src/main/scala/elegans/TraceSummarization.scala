package elegans

import Cells._
import Constraints._
import Experiments._
import Invariants._

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}
import scala.util.Random

object TraceSummarization {
  private def portValues2int(vs: Seq[Boolean]): Int = {
    val idx = vs.lastIndexOf(true)
    idx + 1
  }

  private def extractEvents(trace: History, cells: Seq[Cell]):
      MutableMap[Cell, MutableMap[Int, Set[Event]]] = {
    val eventsPerCell = MutableMap[Cell, MutableMap[Int, Set[Event]]]()

    for (cell <- cells) {
      eventsPerCell(cell) = MutableMap[Int, Set[Event]]()
      for ((t, portValues) <- trace) {
        eventsPerCell(cell)(t) = Set[Event]()
        // register concentration changes at this timestep
        for (node <- cell.N) {
          for (PortBundle(id, ports) <- node.outputBundles) {
            val currentPortValues = ports map (portValues(_))
            val newEvent = ConcentrationChangeEvent(cell.name, 
              node.name, id, portValues2int(currentPortValues))
            if (t > 0) {
              val previousPortValues = ports map (trace(t-1)(_))
              if (currentPortValues != previousPortValues) {
                eventsPerCell(cell)(t) += newEvent
              }
            } else {
              //eventsPerCell(cell)(t) += newEvent
            }
          }
        }
      }
    }

    eventsPerCell
  }

  //the 3 strings are cellID, nodeID, and bundleID
  private def extractValues(trace: History, cells: Seq[Cell]):
      MutableMap[String, MutableMap[(String, String), MutableMap[Int, Int]]] = {
    val traceTimeSize = trace.keySet.max
    //Cell to PortBundle to Time to Value at the port
    val extractedValues = MutableMap[String, MutableMap[(String, String), MutableMap[Int, Int]]]()

    for (cell <- cells) {
      var valuesPerCell = MutableMap[(String, String), MutableMap[Int, Int]]() 
      for (node <- cell.N) {
        for (pB @ PortBundle(id, ports) <- node.outputBundles) {
          var valuesPerPort = MutableMap[Int, Int]()
          for (t <- 0 until traceTimeSize) {
            val value = portValues2int(ports.map(trace(t)(_)))
            valuesPerPort += ((t,value))
          }
          valuesPerCell += (((node.name, id), valuesPerPort))
        }
      }
      extractedValues += ((cell.toString, valuesPerCell))
    }
    extractedValues
  }

  private def allHappenedBeforeCombinations(eventSets: Seq[Set[Event]]):
      Set[Invariant] = eventSets match {
    case Seq() => Set[Invariant]()
    case Seq(s, rest @ _*) =>
      val allPairs = s flatMap {
        event =>
          rest flatMap {
            futureSet => futureSet map {
              futureEvent => HappenedBefore(event, futureEvent)
            }
          }
      }
      allPairs.toSet ++ allHappenedBeforeCombinations(rest)
  }

  private def allHappenedBeforeCombinationsBetweenCells(eventSetPairs: Seq[(Set[Event], Set[Event])]):
      Set[Invariant] = eventSetPairs match {
    case Seq() => Set[Invariant]()
    case Seq((set1, set2), rest @ _*) =>
      val from1to2 = set1 flatMap {
        event1 => {
          rest flatMap {
            case (_, futureSet2) => futureSet2 map {
              futureEvent => HappenedBefore(event1, futureEvent)
            }
          }
        }
      }
      val from2to1 = set2 flatMap {
        event2 => {
          rest flatMap {
            case (futureSet1, _) => futureSet1 map {
              futureEvent => HappenedBefore(event2, futureEvent)
            }
          }
        }
      }
      from1to2.toSet ++ from2to1.toSet ++ allHappenedBeforeCombinationsBetweenCells(rest)
  }

  private def constructInvariants(
      cells: Seq[Cell], 
      eventsPerCell: MutableMap[Cell, MutableMap[Int, Set[Event]]]): 
      (MutableMap[String, Set[Invariant]], MutableMap[Set[String], Set[Invariant]]) = {
    val invariantsPerCell = MutableMap[String, Set[Invariant]]()

    for (cell <- cells) {
      val events = eventsPerCell(cell)
      val orderedPerTime = events.toList.sortBy(_._1).map(_._2)
      
      invariantsPerCell(cell.name) = allHappenedBeforeCombinations(orderedPerTime)
    }

    val invariantsPerCellPair = MutableMap[Set[String], Set[Invariant]]()

    for (cell <- cells) {
      for (neighbor <- neighbors(cell)) {
        invariantsPerCellPair.get(Set(cell.name, neighbor.name)) match {
          case Some(_) => // already computed
          case None => 
            val thisCellEvents = eventsPerCell(cell)
            val otherCellEvents = eventsPerCell(neighbor)
            val timeIndices = thisCellEvents.keySet ++ otherCellEvents.keySet
            val orderedIndices = timeIndices.toList.sortBy(x => x)
            val orderedEventSetPairs = orderedIndices map {
              t => 
                (thisCellEvents.getOrElse(t, Set[Event]()), otherCellEvents.getOrElse(t, Set[Event]()))
            }
            invariantsPerCellPair(Set(cell.name, neighbor.name)) = allHappenedBeforeCombinationsBetweenCells(orderedEventSetPairs)
        }
      }
    }

    (invariantsPerCell, invariantsPerCellPair)
  }

  def summarizeTrace(trace: History, cells: Seq[Cell]): 
      (MutableMap[String, Set[Invariant]],
       MutableMap[Set[String], Set[Invariant]]) = {
    // summarize invariants for each cell
    val eventsPerCell = extractEvents(trace, cells)
    constructInvariants(cells, eventsPerCell)
  }

  def summarizeExperiment(experiment: Experiment, solution: Option[Solution]) = {
    // first get invariants on synchronous schedule
    val Some((cells, history)) = Settings.runningMethod(experiment, 
      Schedules.syncCoarseSchedule(Model.nbAsyncCells, Settings.runLength), solution)

    val (invariantsPerCell, invariantsPerCellPair) = summarizeTrace(history, cells)
    var intersectedInvariants: Set[Invariant] = 
      invariantsPerCell.foldLeft[Set[Invariant]](Set[Invariant]())(_ ++ _._2) ++
      invariantsPerCellPair.foldLeft[Set[Invariant]](Set[Invariant]())(_ ++ _._2)

    // then prune them with more schedules
    val nbAttempts = 10
    var nbMissesRemaining = nbAttempts

    while (nbMissesRemaining > 0) {
      log("Considering new schedule for pruning invariants.")
      var prunedAtThisStep = false

      val randomSchedule = Schedules.randomMacroStepSchedule(Model.nbAsyncCells, 10)

      val Some((newCells, newHistory)) = Settings.runningMethod(experiment,
        randomSchedule, solution)

      val (newInvariantsPerCell, newInvariantsPerCellPair) = summarizeTrace(newHistory, newCells)
      var newInvariants: Set[Invariant] = 
        newInvariantsPerCell.foldLeft[Set[Invariant]](Set[Invariant]())(_ ++ _._2) ++
        newInvariantsPerCellPair.foldLeft[Set[Invariant]](Set[Invariant]())(_ ++ _._2)

      val intersection = intersectedInvariants intersect newInvariants
      if (intersection != intersectedInvariants) {
        prunedAtThisStep = true
      } 
      intersectedInvariants = intersection

      if (prunedAtThisStep) {
        nbMissesRemaining = nbAttempts
        log("Pruned, counter reset to " + nbMissesRemaining)
      } else {
        nbMissesRemaining -= 1
        log("Nothing pruned with this schedule, counter down to " + 
          nbMissesRemaining)
      }
    }

    val aTrace = extractValues(history, cells)
    val prunedInvars = pruneRedundantInvariants(intersectedInvariants)
    val totalTime = history.keySet.max

    def logGnuPlot(trace : MutableMap[String, MutableMap[(String, String), MutableMap[Int, Int]]], 
      invars : Set[Invariant], totalTime : Int) = {

      //logs the header
      log("set terminal png")
      log("set output \"waveformTrace.png\"")
      log("set title  \"waveformTrace\"")
      log("set terminal png size 2600,2600")
      log("set samples 10000")
      log("set nokey")
      log("set label \"Arial Bold\"")
      log("set style line 100 lw 2 lc rgb \'#bbbbbb\'")


      //extract some basic keySets... component is form (nodeN,bundN), cell is form cellN
      var components = Set[(String, String)]()
      var cells = Set[String]()

      for ((cellN,insideCell) <- trace){
        for (((nodeN,bundN),whocares) <- insideCell){
          cells += cellN
          components += ((nodeN,bundN))
        }
      }

      //sort so it prints consistently
      val sortedComponents = components.toList.sorted
      val sortedCells = cells.toList.sorted

      def randomColor() : String = {
        val literals : List[String] = List("0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f")
        var ret : String = "'#"
        for (i <- 0 until 6){
          ret += Random.shuffle(literals).head
        }
        ret + "'"
      }

      //for each cell, set a style(color) and log it
      //1000 is a magic number
      var cellToColorMap = MutableMap[String, Int]()
      for ((cellN,idx) <- sortedCells.reverse.zipWithIndex){
        val id = idx+1
        val color = randomColor()
        cellToColorMap += ((cellN,id))
        //the cell's line color
        log("set style line "+id+" lw 3 lc rgb " + color)
        //the arrow's lien color is the same, but thinner
        log("set style line "+(id+1000)+" lw 1 lc rgb " + color)
        //generate a lable that's the same as the cell's color
        log("set label \""+cellN+"_COLOR\" at "+ 0 + ", " + (idx*(-1)+4*(sortedComponents.size+1)) + " tc rgb " + color) 
      }

      //for each chemical, we allocate a fake x axis with some fixed BaseHeight offset
      var chemToBaseHeight = MutableMap[(String, String), Int]()
      for ((comp,idx) <- sortedComponents.zipWithIndex){
        chemToBaseHeight += ((comp, idx * 4))
      }

      //for each cell that run on this chemical, differentiate diff cell by a DeltaHeight
      var cellToDeltaHeight = MutableMap[String, Double]()
      for ((cell,idx) <- sortedCells.zipWithIndex){
        //each cell on a different delta height, but tallest one is no more than 0.7
        cellToDeltaHeight += ((cell, idx.toFloat * 0.7 / sortedCells.size))
      }

      case class Func(name: String, associatedCell: Option[String])

      var setOfFunctions = Set[Func]()

      //finally we log the function... for each chemical(component), there are ALL the cells
      //running on it, if the cell contains that chemical
      for (compo <- sortedComponents){
        log("#the base line for "+compo+" is at "+chemToBaseHeight(compo))
        val nId = compo._1
        val bId = compo._2
        log("set label \""+nId+"_"+bId+"\" at "+ 0 + ", " + chemToBaseHeight(compo)) 
        for (cell <- sortedCells){
          if (trace(cell).contains((nId,bId))){
            val baseHeight = chemToBaseHeight(compo) + cellToDeltaHeight(cell)
            val funcName = cell+"_"+nId+"_"+bId
            setOfFunctions += Func(funcName,Some(cell))
            log(funcName+"(x) = \\")
            for (x <- 0 until totalTime){
              val funcHeight = trace(cell)((nId,bId))(x) + baseHeight
              log("x < " + (x+1).toString + " ? " + funcHeight.toString + " :\\")
            }
            log ("0\n")
          }
        }
      }

      //give some event, find out explicitly in the trace what time it happend
      def findEventTime(cId: String, nId: String, bId: String, conc: Int) : Int = {
        val theThing = trace(cId)((nId,bId)).toList.sorted
        val foundTime = theThing.find(x => x._2 == conc) match {
          case Some((time,concen)) => time
          case _ => -1
        }
        foundTime
      }

      //find and draw the arrows
      for (inv <- invars){
        inv match {
          case HappenedBefore(e1, e2) => {
            (e1,e2) match {
              case(ConcentrationChangeEvent(cId1,nId1,bId1,conc1),
                   ConcentrationChangeEvent(cId2,nId2,bId2,conc2)) => {
                   //et stands for event time
                   val et1 = findEventTime(cId1,nId1,bId1,conc1)+0.03
                   val et2 = findEventTime(cId2,nId2,bId2,conc2)+0.03
                   //efn stands for event's function name
                   val efn1 = cId1+"_"+nId1+"_"+bId1
                   val efn2 = cId2+"_"+nId2+"_"+bId2
                   //also use magic number
                   val lineStyle = cellToColorMap(cId1)+1000
                   log("set arrow from "+et1+","+efn1+"("+et1+") to "
                                        +et2+","+efn2+"("+et2+") nohead ls "+lineStyle)
                 }
            }
          }
        }

      }

      //plot out the functions...
      log("plot " + "[0:" + (totalTime - 0.1) + "]\\")
      for (func @ Func(name, ascCell) <- setOfFunctions){
        ascCell match {
          case Some(cellN) => {
            val lineStyleNum = cellToColorMap(cellN)
            log(name+"(x) ls "+lineStyleNum+" ,\\")
          }
          case None => {}
        }
      }
    }

    logGnuPlot(aTrace, prunedInvars,totalTime)
    //logDotString(prunedInvars)
    //logDotString(intersectedInvariants)
  }

  def logDotString (prunedInvars : Set[Invariant]) : Unit = {
    log("Final invariants as dot:")
    log("digraph {\n" + 
        "  rankdir = LR\n")
    log(prunedInvars.map( x =>
        x match {
          case hb @ HappenedBefore(_,_) => hb.toDotString
        }
      ).mkString("\n"))
    log("}\n")
  }

  private def implies(assumptions: Set[Invariant], 
      invariantToTest: Invariant): Boolean = {
    import z3.scala._

    val ctx = new Z3Context()

    val z3Vars = MutableMap[Event, Z3AST]()

    def event2Z3AST(e: Event): Z3AST = z3Vars.get(e) match {
      case Some(ast) => ast
      case None =>
        val fresh = ctx.mkFreshIntConst(e.toString)
        z3Vars(e) = fresh
        fresh
    }

    def invariant2Z3AST(i: Invariant): Z3AST = i match {
      case HappenedBefore(e1, e2) => ctx.mkLT(event2Z3AST(e1), event2Z3AST(e2))
    }

    val assumptionsAST = assumptions.map(invariant2Z3AST(_)).foldLeft (ctx.mkTrue)(ctx.mkAnd(_, _))
    val redundancyCnstr = 
      ctx.mkImplies(assumptionsAST, invariant2Z3AST(invariantToTest))

    ctx.assertCnstr(ctx.mkNot(redundancyCnstr))

    val toRet = ctx.check match {
      case Some(false) => true
      case _ => false
    }

    ctx.delete

    toRet
  }

  private def pruneRedundantInvariants(invariants: Set[Invariant]): Set[Invariant] = {
    var intraCellInvars = Set[Invariant]()
    var interCellInvars = Set[Invariant]()

    for (invar <- invariants){
      invar match {
        case hb @ HappenedBefore(e1, e2) => {
          (e1, e2) match {
            case (ConcentrationChangeEvent(cell1,node1,bundle1,concen1),
                  ConcentrationChangeEvent(cell2,node2,bundle2,concen2)) => {
                  if (cell1 == cell2) intraCellInvars += hb
                  else interCellInvars += hb
                }
          }
        }
      }
    }

    assert(intraCellInvars.size + interCellInvars.size == invariants.size)
    def pruneRedundantInvariants_(invariants: Set[Invariant]): Set[Invariant] = {
      var cannotBePruned  = Set[Invariant]()
      var canBePruned     = Set[Invariant]()
      var canStillPrune = true
      while (canStillPrune) {
        log("Pruning invariants...")
        val assumptionsToUse = invariants -- canBePruned
        val toTest = (invariants -- (cannotBePruned ++ canBePruned)).toList
  
        val idx = toTest.indexWhere{
             invariant =>
                 implies(assumptionsToUse - invariant, invariant)
        } 
                                                                          
        if (idx == -1) {
          log("No invariants to prune!")
          canStillPrune = false
        } else {
          cannotBePruned ++= toTest.take(idx)
          canBePruned += toTest(idx)
          log(idx.toString + " invariants not prunable at this step")
        }
      }
      cannotBePruned
    }
    val allPrunedInvars = pruneRedundantInvariants_(intraCellInvars ++ interCellInvars)
    val intraPrunedInvars = pruneRedundantInvariants_(intraCellInvars)
    allPrunedInvars ++ intraPrunedInvars
  }

  def summarizeExperiments(experiments: Seq[Experiment], 
      solution: Option[Solution]) = {
    for (e <- experiments)
      summarizeExperiment(e, solution)
  }
}
