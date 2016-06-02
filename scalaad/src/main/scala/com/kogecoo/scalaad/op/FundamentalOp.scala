package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply0, Apply1, Apply2, VE}
import com.kogecoo.scalaad.{S0, Shape, Tensor}


case object Zero extends NullaryOp[S0]

case object One extends NullaryOp[S0]

case object Half extends NullaryOp[S0]

case class Const(v: Tensor[S0]) extends NullaryOp[S0]


case object Pos extends Op0 {

  override def deriv[VS <: Shape](v: VE[VS]): VE[VS] = Apply1(v, Pos)

}

case object Neg extends Op0 {

  override def deriv[VS <: Shape](v: VE[VS]): VE[VS] = Apply1(v, Neg)

}

case object Identity extends Op0 {

  override def deriv[VS <: Shape](v: VE[VS]): VE[VS] = v

}


case object Add extends Op00 {

  override def deriv[S <: Shape](l: VE[S], r: VE[S]): (VE[S], VE[S]) = {
    (Apply0[S](One), Apply0[S](One))
  }

}

case object Sub extends Op00 {

  override def deriv[S <: Shape](l: VE[S], r: VE[S]): (VE[S], VE[S]) = {
    (Apply0(One), Apply1(Apply0(One), Neg))
  }

}

case object Mul extends Op00 {

  override def deriv[S <: Shape](l: VE[S], r: VE[S]): (VE[S], VE[S]) = {
    (r, l)
  }

}

case object Div extends Op00 {

  override def deriv[S <: Shape](l: VE[S], r: VE[S]): (VE[S], VE[S]) = {
    val first = Apply2(Apply0(One), r, Div)
    val second = Apply2(l, Apply2[S](r, r, Mul), Div)
    (first, second)
  }

}

case object Div extends {

  override def deriv[S <: Shape](l: VE[S], r: VE[S]): (VE[S], VE[S]) = {
    val first = Apply2(Apply0(One), r, Div)
    val second = Apply2(l, Apply2[S](r, r, Mul), Div)
    (first, second)
  }

}

