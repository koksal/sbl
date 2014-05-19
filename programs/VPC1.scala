package elegans

object Model {

  import Cells._
  import Experiments._
  import Settings._
  import TimerLogic._

  val nbAsyncCells = 6

  val nbLet23States = 3
  val nbLstStates   = 3

  class Let23 extends Node("let23") {
    val ac          = input("ac")("low", "med", "high")(Constant)
    val hyp         = input("hyp")("on")(Constant)
    val out         = output("out")("on")

    val stateful = logic(new StatefulLogic {
      val off   = state("off")
      val high  = state("on")
      
      init(off)

      nbStates(nbLet23States)
      
      activating(ac)
      activating(hyp)

      output(out)
    })

    register(stateful)
  }

  class Sem5 extends Node("sem5") {
    val let23 = input("let23")("on")
    val lst   = input("lst")("on")

    val out   = output("out")("on")

    val timer = logic(TimerLogic(out)(lst || let23)(
      !lst && let23
    ))

    register(timer)
  }

  class Let60 extends Node("let60") {
    val sem5  = input("sem5")("on")
    val lst   = input("lst")("on")

    val out   = output("out")("on")
    
    val timer = logic(TimerLogic(out)(lst || sem5)(
      !lst && sem5
    ))

    register(timer)
  }

  class Mpk1 extends Node("mpk1") {
    val let60 = input("let60")("on")
    val lst   = input("lst")("on")

    val out   = output("out")("on")

    val timer = logic(TimerLogic(out)(lst || let60)(
      !lst && let60
    ))

    register(timer)
  }

  class Lin12(mutation: String) extends Node("lin12") {
    val let23       = input("let23")("on")
    val ls          = input("ls")("on")
    val out         = output("out")("low", "med", "high")

    val statefulGF = logic(new StatefulLogic{
      val off = state("off")  
      val low = state("low")  
      val med = state("med")  
      val high = state("high")  

      init(high)

      nbStates(4)

      output(out)

      val ts = Map(
        ((Seq[Int](), Seq[Int]()), 0) -> 0,
        ((Seq[Int](), Seq[Int]()), 1) -> 1,
        ((Seq[Int](), Seq[Int]()), 2) -> 2,
        ((Seq[Int](), Seq[Int]()), 3) -> 3
      )

      val ms = Map(
        0 -> 0,
        1 -> 1,
        2 -> 2,
        3 -> 3
      )

      val is = 3

      setImplementation(ts, ms, is)
    })

    val statefulWT = logic(new StatefulLogic {
      val off = state("off")  
      val low = state("low")  
      val med = state("med")  
      val high = state("high")  

      init(med)
      
      nbStates(6)

      activating(ls)
      inhibiting(let23)

      output(out)

      val ts = Map(
        // ls, let23, state  -> state'

        // off
        ((Seq(0), Seq(0)), 0) -> 0,
        ((Seq(0), Seq(1)), 0) -> 0,
        ((Seq(1), Seq(0)), 0) -> 0,
        ((Seq(1), Seq(1)), 0) -> 0,

        // low
        ((Seq(0), Seq(0)), 1) -> 1,
        ((Seq(0), Seq(1)), 1) -> 1,
        ((Seq(1), Seq(0)), 1) -> 1,
        ((Seq(1), Seq(1)), 1) -> 1,

        // med1
        ((Seq(0), Seq(0)), 2) -> 1,
        ((Seq(0), Seq(1)), 2) -> 1,
        ((Seq(1), Seq(0)), 2) -> 5,
        ((Seq(1), Seq(1)), 2) -> 5,

        // med2
        ((Seq(0), Seq(0)), 3) -> 2,
        ((Seq(0), Seq(1)), 3) -> 1,
        ((Seq(1), Seq(0)), 3) -> 5,
        ((Seq(1), Seq(1)), 3) -> 5,

        // med3
        ((Seq(0), Seq(0)), 4) -> 3,
        ((Seq(0), Seq(1)), 4) -> 1,
        ((Seq(1), Seq(0)), 4) -> 5,
        ((Seq(1), Seq(1)), 4) -> 5,

        // high
        ((Seq(0), Seq(0)), 5) -> 5,
        ((Seq(0), Seq(1)), 5) -> 5,
        ((Seq(1), Seq(0)), 5) -> 5,
        ((Seq(1), Seq(1)), 5) -> 5
      )

      val ms = Map(
        0 -> 0,
        1 -> 1,
        2 -> 2,
        3 -> 2,
        4 -> 2,
        5 -> 3
      )

      val is = 4

      setImplementation(ts, ms, is)
    })

    if (mutation == "gf") {
      register(statefulGF)
    } else {
      register(statefulWT)
    }
  }

  class Lst extends Node("lst") {
    val lin12       = input("lin12")("low", "med", "high")
    val ls          = input("ls")("on")(Monotonic)
    val let23       = input("let23")("on")

    val out         = output("out")("on")

    val stateful = logic(new StatefulLogic {
      val off = state("off")  
      val on  = state("on")

      init(off)

      nbStates(nbLstStates)

      activating(lin12)
      activating(ls)
      inhibiting(let23)

      output(out)
    })

    register(stateful)
  }

  class Ls extends Node("ls") {
    val ls_left   = input("ls_left")("on")
    val ls_right  = input("ls_right")("on")
    val let23      = input("let23")("on")

    val out = output("out")("on")

    val timer = logic(TimerLogic(out)(ls_left || ls_right || let23)(
      !(ls_left || ls_right) && let23
    ))

    register(timer)
  }

  class Fate11 extends Node("fate11") {
    val let23     = input("let23")("on")

    val out = output("out")("on")

    val timer = logic(TimerLogic(out)(true)(
      let23,
      let23,
      let23,
      let23,
      let23
    ))
    register(timer)

  }

  class Fate12 extends Node("fate12") {
    val let23     = input("let23")("on")

    val out = output("out")("on")

    val timer = logic(TimerLogic(out)(true)(
      false,
      false,
      false,
      false,
      !let23
    ))
    register(timer)

  }

  class Primary extends Node("primary") {
    val fate11    = input("fate11")("on")
    val mpk1      = input("mpk1")("on")

    val out       = output("out")("on")

    outcome(out, "1")

    val timer = logic(TimerLogic(out)(fate11 || mpk1)(
      false,
      false,
      false,
      false,
      mpk1
    ))
    register(timer)
  }

  class Secondary1 extends Node("secondary1") {
    val fate11    = input("fate11")("on")
    val mpk1      = input("mpk1")("on")

    val out       = output("out")("on")

    outcome(out, "2")

    val timer = logic(TimerLogic(out)(fate11 || mpk1)(
      false,
      false,
      false,
      false,
      !mpk1
    ))
    register(timer)
  }

  class Secondary2 extends Node("secondary2") {
    val fate12      = input("fate12")("on")
    val lin12       = input("lin12")("low", "med", "high")

    val out         = output("out")("on")

    outcome(out, "2")

    // TODO: refer to bundle values instead of indexing ports
    val timer = logic(TimerLogic(out)(fate12)(
      false,
      false,
      false,
      false,
      lin12.ports(2)
    ))
    register(timer)
  }

  class Tertiary extends Node("tertiary") {
    val fate12      = input("fate12")("on")
    val lin12       = input("lin12")("low", "med", "high")

    val out         = output("out")("on")

    outcome(out, "3")

    // TODO: refer to bundle values instead of indexing ports
    val timer = logic(TimerLogic(out)(fate12)(
      false,
      false,
      false,
      false,
      !lin12.ports(2) || lin12.ports(0)
    ))
    register(timer)
  }

  class VPC(name: String, experiment: Experiment) extends Cell(name) {
    val let23       = node(new Let23())
    val sem5        = node(new Sem5())
    val let60       = node(new Let60())
    val mpk1        = node(new Mpk1())
    val lin12       = node(new Lin12(experiment.mutations("lin12")))
    val lst         = node(new Lst())
    val ls          = node(new Ls())
    val fate11      = node(new Fate11())
    val fate12      = node(new Fate12())
    val primary     = node(new Primary())
    val secondary1  = node(new Secondary1())
    val secondary2  = node(new Secondary2())
    val tertiary    = node(new Tertiary())

    // connections from let23
    if (experiment.mutations("let23") == "wt") {
      let23.out --> fate11.let23
      let23.out --> fate12.let23

      let23.out --> sem5.let23
      let23.out --> ls.let23

      let23.out --| lst.let23
      let23.out --| lin12.let23
    }

    // connections from sem5
    if (experiment.mutations("sem5") == "wt") {
      sem5.out --> let60.sem5
      // sem5.out --> ls.sem5
    }

    // connections from let60
    if (experiment.mutations("let60") == "wt") {
      let60.out --> mpk1.let60
    }

    // connections from mpk1
    if (experiment.mutations("mpk1") == "wt") {
      mpk1.out --> primary.mpk1
      mpk1.out --> secondary1.mpk1
    }

    if (experiment.mutations("lst") == "wt") {
      // lst inhibits the protein cascade
      lst.out --| sem5.lst
      lst.out --| let60.lst
      lst.out --| mpk1.lst
    }

    // connections from lin12
    if (experiment.mutations("lin12") != "ko") {
      lin12.out --> lst.lin12
      lin12.out --> secondary2.lin12
      lin12.out --> tertiary.lin12
    }

    // connections from ls
    ls.out --> lst.ls
    ls.out --> lin12.ls

    // connections from fate11
    fate11.out --> primary.fate11
    fate11.out --> secondary1.fate11

    // connections from fate12
    fate12.out --> secondary2.fate12
    fate12.out --> tertiary.fate12
  }

  private def createVPCs(experiment: Experiment): List[VPC] = {
    val vpcs = for (i <- 1 to nbAsyncCells) yield
      new VPC("VPC_" + i, experiment)

    // connect cells together
    vpcs.reduceLeft[VPC] { case (left, right) =>
      left.ls.out   --> right.lin12.ls
      left.ls.out   --> right.lst.ls

      right.ls.out  --> left.lin12.ls
      right.ls.out  --> left.lst.ls

      left.ls.out   --> right.ls.ls_left
      right.ls.out  --> left.ls.ls_right

      right
    }

    vpcs.toList
  }

  class ACNode extends Node("acNode") {
    val out = output("out")("on")

    val timer = logic(TimerLogic(out)(true) (
      true
    ))

    register(timer)
  }

  class AC extends Cell("AC") {
    val acNode = node(new ACNode)
  }

  class HypNode extends Node("hypNode") {
    val muv = input("muv")("on")
    val out = output("out")("on")

    val timer = logic(TimerLogic(out)(true)(
      !muv  
    ))
    register(timer)
  }

  class Hyp extends Cell("Hyp") {
    val hypNode = node(new HypNode)
  }

  class MuvNode extends Node("muvNode") {
    val out = output("out")("on")
    val timer = logic(TimerLogic(out)(true)(
      true  
    ))
    register(timer)
  }

  class Muv extends Cell("Muv") {
    val muvNode = node(new MuvNode)
  }

  /** Creates list of non-VPC cells, apriori channels with these cells, and VPCs */
  def createSystem(experiment: Experiment): (List[Cell], List[(Cell, Cell)], List[Cell]) = {
    val vpcs = createVPCs(experiment)
    val ac = new AC
    val muv = new Muv
    val hyp = new Hyp

    // muv-hyp inhibition
    if (experiment.mutations("lin15") == "wt")
      muv.muvNode.out --| hyp.hypNode.muv

    // hyp-vpc activation
    for (vpc <- vpcs)
      hyp.hypNode.out --> vpc.let23.hyp

    // ac-vpc actication
    if (experiment.mutations("ac") == "Formed") {
      ac.acNode.out.ports(0) --> vpcs(0).let23.ac.ports(0)
      ac.acNode.out.ports(0) --> vpcs(1).let23.ac.ports(0)
      ac.acNode.out.ports(0) --> vpcs(2).let23.ac.ports(1)
      ac.acNode.out.ports(0) --> vpcs(3).let23.ac.ports(2)
      ac.acNode.out.ports(0) --> vpcs(4).let23.ac.ports(1)
      ac.acNode.out.ports(0) --> vpcs(5).let23.ac.ports(0)
    } else {
      ac.acNode.out.ports(0) --> vpcs(0).let23.ac.ports(0)
      ac.acNode.out.ports(0) --> vpcs(1).let23.ac.ports(0)
      ac.acNode.out.ports(0) --> vpcs(2).let23.ac.ports(0)
      ac.acNode.out.ports(0) --> vpcs(3).let23.ac.ports(0)
      ac.acNode.out.ports(0) --> vpcs(4).let23.ac.ports(0)
      ac.acNode.out.ports(0) --> vpcs(5).let23.ac.ports(0)
    }

    val acChannels = for (vpc <- vpcs) yield (ac, vpc)
    val hypChannels = for (vpc <- vpcs) yield (hyp, vpc)
    val muvChannel = (muv, hyp)

    val aPrioriChannels = muvChannel :: acChannels.toList ::: hypChannels.toList
    
    (List(ac, muv, hyp), aPrioriChannels, vpcs)
  }
}
