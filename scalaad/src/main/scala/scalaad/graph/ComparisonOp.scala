package scalaad.graph


case class Eq(override val l: DExpr, override val r: DExpr) extends Elementwise2

case class Neq(override val l: DExpr, override val r: DExpr) extends Elementwise2

case class Lt(override val l: DExpr, override val r: DExpr) extends Elementwise2

case class Lte(override val l: DExpr, override val r: DExpr) extends Elementwise2

case class Gt(override val l: DExpr, override val r: DExpr) extends Elementwise2

case class Gte(override val l: DExpr, override val r: DExpr) extends Elementwise2
