package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{If, One, V, Where, Zero}
import com.kogecoo.scalaad.op.Shorthands.Const._
import com.kogecoo.scalaad.op.Shorthands.Math._
import shapeless.Nat


// Expr[Shape0] -> Expr[Shape0]

case object Sin extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = cos(v)

}

case object Cos extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = -sin(v)

}

case object Tan extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = one(v) - (tan(v) * tan(v))

}

case object Asin extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = one(v) / sqrt(one(v) - (v * v))

}

case object Acos extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = -(one(v) / sqrt(one(v) - (v * v)))

}

case object Atan extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = one(v) :/ (one(v) :+ (v :* v))

}

case object Sinh extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = cosh(v)

}

case object Cosh extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = sinh(v)

}

case object Tanh extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = one(v) - (tan(v) * tan(v))

}

case object Ln extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = one(v) / v
}

case object Exp extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = exp(v)

}

case object Sqrt extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = half(v) / sqrt(v)
}

case object Abs extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    Where[N](v >= zero(v), one(v), -one(v))
  }

}


// (Expr[Shape0], Expr[Shape0]) -> Expr[Shape0]

case object Pow extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    val dl = ln(l) :* pow(l, r)
    val dr = r :* pow(l, r :- one(r))
    (dl, dr)
  }

}

case object Max extends BinaryOp {

  def deriv[N <: Nat](l: V[N], r: V[N]): (V[N], V[N]) = {
    (If[N](l >= r, one(l)), If[N](l < r, one(r)))
  }

}

case object Min extends BinaryOp {

  def deriv[N <: Nat](l: V[N], r: V[N]): (V[N], V[N]) = {
    (If[N](l <= r, one(l)), If[N](l > r, one(r)))
  }

}


// Expr[Shape1] -> Expr[Shape0]

case object L0Norm extends UnaryFoldOp

case object L1Norm extends UnaryFoldOp

case object L2Norm extends UnaryFoldOp

case object Sum1 extends UnaryFoldOp

case object Max1 extends UnaryFoldOp

case object Min1 extends UnaryFoldOp


// Expr[Shape2] -> Expr[Shape0]

case object Sum2 extends UnaryFoldOp

case object Max2 extends UnaryFoldOp

case object Min2 extends UnaryFoldOp


// (Expr[Shape1], Expr[Shape1]) -> Expr[Shape0]

case object Dot extends BinaryFoldOp


// (Expr[Shape2], Expr[Shape2]) -> Expr[Shape2]

case object MatMul extends BinaryFoldOp
