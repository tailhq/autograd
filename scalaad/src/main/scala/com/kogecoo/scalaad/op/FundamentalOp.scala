package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply0, Apply1, Apply2, ElementwiseLeft, ElementwiseRight, VE}
import com.kogecoo.scalaad.{Shape, Tensor}
import shapeless.Nat


case object Zero extends NullaryOp

case object One extends NullaryOp

case object Half extends NullaryOp

case class Const[N <: Nat](v: Tensor[N]) extends NullaryOp


case object Pos extends UnaryOp {

  override def deriv[VS <: Nat](v: VE[VS]): VE[VS] = Apply1(v, Pos)

}

case object Neg extends UnaryOp {

  override def deriv[VS <: Nat](v: VE[VS]): VE[VS] = Apply1(v, Neg)

}

case object Identity extends UnaryOp {

  override def deriv[VS <: Nat](v: VE[VS]): VE[VS] = v

}


case object Add extends BinaryOp {

  override def deriv[N <: Nat](l: VE[N], r: VE[N]): (VE[N], VE[N]) = {
    (Apply0[N](l.shape, One), Apply0[N](r.shape, One))
  }

}

case object AddLeft extends AsymmetricLeftBinaryOp {

  override def deriv[N <: Nat, M <: Nat](l: VE[N], r: VE[M]): (VE[N], VE[N]) = {
    (Apply0[N](l.shape, One), Apply0[N](l.shape, One))
  }

}

case object AddRight extends AsymmetricRightBinaryOp {

  override def deriv[N <: Nat, M <: Nat](l: VE[N], r: VE[M]): (VE[M], VE[M]) = {
    (Apply0[M](r.shape, One), Apply0[M](r.shape, One))
  }

}

case object Sub extends BinaryOp {

  override def deriv[N <: Nat](l: VE[N], r: VE[N]): (VE[N], VE[N]) = {
    (Apply0[N](l.shape, One), Apply1(Apply0(r.shape, One), Neg))
  }

}

case object SubLeft extends AsymmetricLeftBinaryOp {

  override def deriv[N <: Nat, M <: Nat](l: VE[N], r: VE[M]): (VE[N], VE[N]) = {
    (Apply0[N](l.shape, One), Apply1[N](Apply0(l.shape, One), Neg))
  }

}

case object SubRight extends AsymmetricRightBinaryOp {

  override def deriv[N <: Nat, M <: Nat](l: VE[N], r: VE[M]): (VE[M], VE[M]) = {
    (Apply0[M](r.shape, One), Apply1[M](Apply0(r.shape, One), Neg))
  }

}

case object Mul extends BinaryOp {

  override def deriv[N <: Nat](l: VE[N], r: VE[N]): (VE[N], VE[N]) = {
    (l, r)
  }

}

case object MulLeft extends AsymmetricLeftBinaryOp {

  override def deriv[N <: Nat, M <: Nat](l: VE[N], r: VE[M]): (VE[N], VE[N]) = {
    (l, r)
  }

}

case object MulRight extends AsymmetricRightBinaryOp {

  override def deriv[N <: Nat, M <: Nat](l: VE[N], r: VE[M]): (VE[M], VE[M]) = {
    (l, r)
  }

}

case object Div extends BinaryOp {

  override def deriv[N <: Nat](l: VE[N], r: VE[N]): (VE[N], VE[N]) = {
    val first = Apply2(Apply0(l.shape, One), r, Div)
    val second = Apply2(l, Apply2[N](r, r, Mul), Div)
    (first, second)
  }

}

case object DivLeft extends AsymmetricLeftBinaryOp {

  override def deriv[N <: Nat, M <: Nat](l: VE[N], r: VE[M]): (VE[N], VE[N]) = {
    val first = ElementwiseLeft(Apply0(l.shape, One), r, DivLeft)
    val second = ElementwiseLeft(l, Apply2[M](r, r, Mul), DivLeft)
    (first, second)
  }

}

case object DivRight extends AsymmetricRightBinaryOp {

  override def deriv[N <: Nat, M <: Nat](l: VE[N], r: VE[M]): (VE[M], VE[M]) = {
    val first = ElementwiseRight(Apply0(l.shape, One), r, DivRight)
    val second = ElementwiseRight(l, Apply2[M](r, r, Mul), DivRight)
    (first, second)
  }

}
