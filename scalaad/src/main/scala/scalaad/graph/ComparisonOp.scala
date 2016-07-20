package scalaad.graph


trait Comparison2[I <: DType] extends Elementwise2[Bool, I]

case class Eq(override val l: DExpr[Real], override val r: DExpr[Real]) extends Comparison2[Real]

case class Neq(override val l: DExpr[Real], override val r: DExpr[Real]) extends Comparison2[Real]

case class Lt(override val l: DExpr[Real], override val r: DExpr[Real]) extends Comparison2[Real]

case class Lte(override val l: DExpr[Real], override val r: DExpr[Real]) extends Comparison2[Real]

case class Gt(override val l: DExpr[Real], override val r: DExpr[Real]) extends Comparison2[Real]

case class Gte(override val l: DExpr[Real], override val r: DExpr[Real]) extends Comparison2[Real]
