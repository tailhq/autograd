package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply1, If, One, V, Where, Zero}
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

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    (If[L](l :>= r, one(l)), If[R](l :< r, one(r)))
  }

}

case object Min extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    (If[L](l :<= r, one(l)), If[R](l :> r, one(r)))
  }

}


// Expr[Shape1] -> Expr[Shape0]

case object L0Norm extends UnaryFoldOp {

  def deriv[N <: Nat](v: V[N]): V[N] = zero(v)

}

case object L1Norm extends UnaryFoldOp {

  def deriv[N <: Nat](v: V[N]): V[N] = sign(v)

}


case object L2Norm extends UnaryFoldOp {

  def deriv[N <: Nat](v: V[N]): V[N] = two[N](v) * v

}

case object Sum1 extends UnaryFoldOp {

  // FIXME
  def deriv[N <: Nat](v: V[N]): V[N] = one(v)

}

case object Max1 extends UnaryFoldOp {

  // FIXME
  def deriv[N <: Nat](v: V[N]): V[N] = one(v)

}

case object Min1 extends UnaryFoldOp {

  def deriv[N <: Nat](v: V[N]): V[N] = one(v)

}


// Expr[Shape2] -> Expr[Shape0]

case object Sum2 extends BinaryFoldOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = (one(l), one(r))

}


case object Max2 extends BinaryFoldOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = (one(l), one(r))

}


case object Min2 extends BinaryFoldOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = (one(l), one(r))

}


// (Expr[Shape1], Expr[Shape1]) -> Expr[Shape0]

case object Dot extends BinaryFoldOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    (one(l), one(r))
  }

}


// (Expr[Shape2], Expr[Shape2]) -> Expr[Shape2]

case object MatMul extends BinaryFoldOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    (one(l), one(r))
  }

}
