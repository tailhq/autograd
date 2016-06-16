package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.Shorthands.const._
import com.kogecoo.scalaad.Shorthands.math._
import com.kogecoo.scalaad.graph.{ElementwiseWhere, V, Where}
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
    ElementwiseWhere[N](v >= zero(v), one(v), -one(v))
  }

}


// Expr[Shape1] -> Expr[Shape0]

/*
case object L2Norm extends UnaryFoldOp {

  def deriv[N <: Nat](v: V[N]): V[N] = two[N](v) * v

}
*/

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


// (Expr[Shape0], Expr[Shape0]) -> Expr[Shape0]

case object Pow extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    val dl = ln(l) :* pow(l, r)
    val dr = r :* pow(l, r :- one(r))
    (dl, dr)
  }

}

case object Max2 extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    (Where(l :>= r, one(l), zero(r)), Where(l :< r, one(r), zero(l)))
  }

}

case object Min2 extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    (Where(l :<= r, one(l), zero(r)), Where(l :> r, one(r), zero(l)))
  }

}


// Expr[Shape2] -> Expr[Shape0]

case object Sum2 extends BinaryFoldOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = (one(l), one(r))

}


// (Expr[Shape1], Expr[Shape1]) -> Expr[Shape0]

case object Dot extends BinaryFoldOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    (one(l), one(r))
  }

}


// (Expr[Shape2], Expr[Shape2]) -> Expr[Shape2]

case object MatMulOp extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    (l, r)
  }

}
