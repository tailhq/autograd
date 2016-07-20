package scalaad.graph


case class Not(override val v: Expr) extends Elementwise1

case class And(override val l: Expr, override val r: Expr) extends Elementwise2

case class Or(override val l: Expr, override val r: Expr) extends Elementwise2
