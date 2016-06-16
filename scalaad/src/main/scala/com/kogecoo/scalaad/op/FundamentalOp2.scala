package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.V
import com.kogecoo.scalaad.Shorthands.const._
import shapeless.Nat


case object Add extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = (one(l), one(r))

}

case object Sub extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = (one(l), -one(r))

}

case object Mul extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[L], V[R]) = (l, r)

}

case object Div extends BinaryOp {

  override def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat]) = {
    val first = one(l) :/ r
    val second = -l :/ (r :* r)
    (first, second)
  }

}
