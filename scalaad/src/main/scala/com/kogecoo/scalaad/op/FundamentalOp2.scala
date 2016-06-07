package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply0, Apply1, Apply2, VE}
import shapeless.Nat


case object Add extends BinaryOp {

  override def deriv[N <: Nat](l: VE[N], r: VE[N]): (VE[N], VE[N]) = {
    (Apply0[N](l.shape, OneOp), Apply0[N](r.shape, OneOp))
  }

}

case object Sub extends BinaryOp {

  override def deriv[N <: Nat](l: VE[N], r: VE[N]): (VE[N], VE[N]) = {
    (Apply0[N](l.shape, OneOp), Apply1(Apply0(r.shape, OneOp), Neg))
  }

}

case object Mul extends BinaryOp {

  override def deriv[N <: Nat](l: VE[N], r: VE[N]): (VE[N], VE[N]) = {
    (l, r)
  }

}

case object Div extends BinaryOp {

  override def deriv[N <: Nat](l: VE[N], r: VE[N]): (VE[N], VE[N]) = {
    val first = Apply2(Apply0(l.shape, OneOp), r, Div)
    val second = Apply2(l, Apply2[N](r, r, Mul), Div)
    (first, second)
  }

}
