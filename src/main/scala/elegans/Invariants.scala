package elegans

object Invariants {
  sealed trait Invariant
  case class HappenedBefore(e1: Event, e2: Event) extends Invariant {
    override def toString: String = {
      e1.toString + " --> " + e2.toString
    }
    def toDotString: String = {
      (e1, e2) match {
        case(ConcentrationChangeEvent(cId1,nId1,bId1,conc1),
             ConcentrationChangeEvent(cId2,nId2,bId2,conc2)) => {
              val colorStr = if (cId1 != cId2) {"[color=\"#009900\"]"} else {""}
              "\"" + e1.toString + "\"" + " -> " + "\"" + e2.toString + "\" "+colorStr
        }
      }
    }
  }

  sealed trait Event
  case class ConcentrationChangeEvent(cellId: String, nodeId: String, 
      bundleId: String, concentration: Int) extends Event {
    override def toString: String = {
      cellId + "_" + nodeId + "_" + bundleId + " := " + concentration
    }
  }
}
