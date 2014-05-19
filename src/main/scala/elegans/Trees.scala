package elegans

import Cells._

object Trees {
  sealed trait Expr
  case class PortVar(port: Port, time: Int) extends Expr
  case object True extends Expr
  case object False extends Expr
  case class Not(expr: Expr) extends Expr
  case class And(e1: Expr, e2: Expr) extends Expr
  case class Or(e1: Expr, e2: Expr) extends Expr
}
