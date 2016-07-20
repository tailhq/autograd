package scalaad


package object graph {

  type DExpr[D <: DType] = Expr[D] with Differentiable[D]

}
