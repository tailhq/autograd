package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply1, VE}
import shapeless.Nat


case object Pos extends UnaryOp {

  override def deriv[VS <: Nat](v: VE[VS]): VE[VS] = Apply1(v, Pos)

}

case object Neg extends UnaryOp {

  override def deriv[VS <: Nat](v: VE[VS]): VE[VS] = Apply1(v, Neg)

}

case object Identity extends UnaryOp {

  override def deriv[VS <: Nat](v: VE[VS]): VE[VS] = v

}
