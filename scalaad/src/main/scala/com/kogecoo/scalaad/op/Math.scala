package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply1, Apply2, Half, One, V}
import shapeless.Nat


// Expr[Shape0] -> Expr[Shape0]

case object Sin  extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = Apply1[N](v, Cos)

}

case object Cos  extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = Apply1[N](Apply1[N](v, Sin), Neg)

}

case object Tan  extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    One(v.shape) - (Apply1[N](v, Tan) * Apply1[N](v, Tan))
  }

}

case object Asin extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    One(v.shape) / Apply1[N](One[N](v.shape) - v * v, Sqrt)
  }

}

case object Acos extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    Apply1(One(v.shape) / Apply1[N](One[N](v.shape) - (v * v), Sqrt), Neg)
  }
}

case object Atan extends  {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    One(v.shape) / (One[N](v.shape) + (v * v))
  }

}

case object Sinh extends  {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    Apply1[N](v, Cosh)
  }

}

case object Cosh extends  {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    Apply1[N](v, Sinh)
  }
}

case object Tanh extends  {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    One(v.shape) - (Apply1[N](v, Tanh) * Apply1[N](v, Tanh))
  }
}

case object Ln   extends  {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    One(v.shape) / v
  }
}

case object Exp  extends  {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    Apply1[N](v, Exp)
  }
}

case object Sqrt extends  {

  def deriv[N <: Nat](v: V[N]): V[N] = {
    Half(v.shape) / Apply1[N](v, Sqrt)
  }
}

case object Abs  extends  {

  def deriv[N <: Nat](v: V[N]): V[N] = {
  }
}


// Expr[Shape1] -> Expr[Shape0]

case class L0Norm[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class L1Norm[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class L2Norm[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class Sum1[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class Max1[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class Min1[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]


// Expr[Shape2] -> Expr[Shape0]

case class Sum2[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class Max2[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class Min2[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]



// (Expr[Shape0], Expr[Shape0]) -> Expr[Shape0]

case object Pow extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {
    val l = Apply1[L](l, Ln) * Apply2[L, R](l, r),
    val r = r * Apply2[L, R](l, r - One(r))
    (l, r)
  }

}

case object Max extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {

  }

}

case object Min extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {

  }
}


// (Expr[Shape1], Expr[Shape1]) -> Expr[Shape0]

case class Dot[O <: Nat, I <: Nat]() extends BinaryFoldOp[O, I, I]


// (Expr[Shape2], Expr[Shape2]) -> Expr[Shape2]

case class MatMul[O <: Nat, I <: Nat]() extends BinaryFoldOp[O, I, I]
