package elegans

object Model {

  import Cells._
  import Experiments.Experiment
  import TimerLogic._

  val nbAsyncCells = 2

  val nbBaseReceiverSt    = 3
  val nbLateralReceiverSt = 2
  val nbDelayNodeSt       = 2

  class BaseReceiver extends Node("BaseReceiver") {
    val base            = input("base")("low", "high")(Constant)
    val lateralReceiver = input("lateralReceiver")("on")
    val out             = output("out")("on")

    val stateful = logic(new StatefulLogic {
      val off   = state("off")
      val on    = state("on")
      
      init(off)

      nbStates(nbBaseReceiverSt)
      
      activating(base)
      inhibiting(lateralReceiver)

      output(out)
    })

    register(stateful)
  }

  /** We might bypass this node */
  class LateralEmitter extends Node("LateralEmitter") {
    val baseReceiver  = input("baseReceiver")("on")
    val out           = output("out")("on")

    val timer = logic(TimerLogic(out)(baseReceiver)(
      true  
    ))

    register(timer)
  }

  class LateralReceiver extends Node("LateralReceiver") {
    val lateralEmitter = input("lateralEmitter")("on")
    val baseReceiver  = input("baseReceiver")("on")
    val out           = output("out")("on")

    val stateful = logic(new StatefulLogic {
      val off = state("off")
      val on  = state("on")

      init(off)

      nbStates(nbLateralReceiverSt)

      activating(lateralEmitter)
      inhibiting(baseReceiver)

      output(out)
    })

    register(stateful)
  }

  /** This might be bypassed as well */
  class DelayNode extends Node("DelayNode") {
    val baseReceiver    = input("baseReceiver")("on")
    val lateralReceiver = input("lateralReceiver")("on")
    val out           = output("out")("on")

    val stateful = logic(new StatefulLogic {
      val off = state("off")
      val on  = state("on")

      init(off)

      nbStates(nbDelayNodeSt)

      activating(baseReceiver)
      inhibiting(lateralReceiver)

      output(out)
    })

    register(stateful)
  }

  class DecisionNode extends Node("DecisionNode") {
    val delayNode = input("delayNode")("on")

    val out_commit = output("out_commit")("on")
    val out_delegate  = output("out_delegate")("on")

    outcome(out_commit, "C")
    outcome(out_delegate, "D")

    val commitLogic = logic(TimerLogic(out_commit)(true)(
      false,
      false,
      false,
      false,
      delayNode
    ))

    register(commitLogic)

    val delegateLogic = logic(TimerLogic(out_delegate)(true)(
      false,
      false,
      false,
      false,
      !delayNode
    ))
  
    register(delegateLogic)
  }

  class Sensor(name: String) extends Cell(name) {
    val baseReceiver = node(new BaseReceiver())
    val lateralEmitter = node(new LateralEmitter())
    val lateralReceiver = node(new LateralReceiver())
    val delayNode = node(new DelayNode())
    val decisionNode = node(new DecisionNode())

    // intra-sensor connections   
    baseReceiver.out --> delayNode.baseReceiver
    baseReceiver.out --> lateralEmitter.baseReceiver
    baseReceiver.out --| lateralReceiver.baseReceiver

    lateralReceiver.out --| baseReceiver.lateralReceiver
    lateralReceiver.out --| delayNode.lateralReceiver

    delayNode.out --> decisionNode.delayNode
  }

  private def createSensors(experiment: Experiment): List[Sensor] = {
    val sensors = for (i <- 1 to nbAsyncCells) yield
      new Sensor("Sensor_" + i)

    // connect cells together
    if (experiment.mutations("radio") == "on") {
      sensors.reduceLeft[Sensor] { case (left, right) =>
        left.lateralEmitter.out --> right.lateralReceiver.lateralEmitter
        right.lateralEmitter.out --> left.lateralReceiver.lateralEmitter

        right
      }
    }

    sensors.toList
  }

  class BaseNode extends Node("BaseNode") {
    val out = output("out")("on")

    val timer = logic(TimerLogic(out)(true) (
      true
    ))

    register(timer)
  }

  class Base extends Cell("Base") {
    val baseNode = node(new BaseNode)
  }

  /** Creates list of non-VPC cells, apriori channels with these cells, and VPCs */
  def createSystem(experiment: Experiment): (List[Cell], List[(Cell, Cell)], List[Cell]) = {
    val sensors = createSensors(experiment)
    val base = new Base

    if (experiment.mutations("n1") == "H") {
      base.baseNode.out.ports(0) --> sensors(0).baseReceiver.base.ports(1)
    } else if (experiment.mutations("n1") == "L") {
      base.baseNode.out.ports(0) --> sensors(0).baseReceiver.base.ports(0)
    }

    if (experiment.mutations("n2") == "H") {
      base.baseNode.out.ports(0) --> sensors(1).baseReceiver.base.ports(1)
    } else if (experiment.mutations("n2") == "L") {
      base.baseNode.out.ports(0) --> sensors(1).baseReceiver.base.ports(0)
    }

    val baseChannels = for (sensor <- sensors) yield (base, sensor)

    val aPrioriChannels = baseChannels.toList
    
    (List(base), aPrioriChannels, sensors)
  }
}
