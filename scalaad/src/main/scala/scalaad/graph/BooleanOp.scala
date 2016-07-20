package scalaad.graph


trait BooleanOp1 extends Elementwise1[Bool, Bool]

trait BooleanOp2 extends Elementwise2[Bool, Bool]

case class Not(override val v: Expr[Bool]) extends BooleanOp1

case class And(override val l: Expr[Bool], override val r: Expr[Bool]) extends BooleanOp2

case class Or(override val l: Expr[Bool], override val r: Expr[Bool]) extends BooleanOp2
