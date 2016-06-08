package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply0, Apply1, Apply2, One, V}
import shapeless.Nat


case object Add extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {
    (One[L](l.shape), One[R](r.shape))
  }

}

case object Sub extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {
    (One[L](l.shape), Apply1[R](One(r.shape), Neg))
  }

}

case object Mul extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {
    (l, r)
  }

}

case object Div extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {
    val first = Apply2[L](One[L](l.shape), r, Div)
    val second = Apply2[R](Apply1[L](l, Neg), Apply2[R](r, r, Mul), Div)
    (first, second)
  }

}
