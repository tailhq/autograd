package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{One, V}
import shapeless.Nat


case object Add extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {
    (One[L](l.shape), One[R](r.shape))
  }

}

case object Sub extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {
    (One[L](l.shape), -One(r.shape))
  }

}

case object Mul extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {
    (l, r)
  }

}

case object Div extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = {
    val first = One[L](l.shape) :/ r
    val second = -l :/ (r :* r)
    (first, second)
  }

}
