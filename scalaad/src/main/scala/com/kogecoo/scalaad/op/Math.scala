package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Unsafe, V}
import com.kogecoo.scalaad.op.Shorthands.Const._
import com.kogecoo.scalaad.op.Shorthands.Math._
import shapeless.Nat


// Expr[Shape0] -> Expr[Shape0]

case object Sin  extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = cos(v)

}

case object Cos  extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = -sin(v)

}

case object Tan  extends UnaryOp {

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

case object Ln   extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = one(v) / v
}

case object Exp  extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = exp(v)

}

case object Sqrt extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = half(v) / sqrt(v)
}
/*
case object Abs  extends UnaryOp {

  def deriv[N <: Nat](v: V[N]): V[N] = {
  }
}
*/


// Expr[Shape1] -> Expr[Shape0]

/*case class L0Norm[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class L1Norm[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class L2Norm[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class Sum1[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class Max1[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class Min1[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]
*/

// Expr[Shape2] -> Expr[Shape0]

/*case class Sum2[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class Max2[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]

case class Min2[O <: Nat, I <: Nat]() extends UnaryFoldOp[O, I]
*/


// (Expr[Shape0], Expr[Shape0]) -> Expr[Shape0]


case object Pow extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    val dl = ln(l) :* pow(l, r)
    val dr = r :* pow(l, r :- one(r))
    (dl, dr)
  }

}
/*
case object Max extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {

  }

}

case object Min extends BinaryOp {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {

  }
}


// (Expr[Shape1], Expr[Shape1]) -> Expr[Shape0]

case class Dot[O <: Nat, I <: Nat]() extends BinaryFoldOp[O, I, I] {

}


// (Expr[Shape2], Expr[Shape2]) -> Expr[Shape2]

case class MatMul[O <: Nat, I <: Nat]() extends BinaryFoldOp[O, I, I]
*/
