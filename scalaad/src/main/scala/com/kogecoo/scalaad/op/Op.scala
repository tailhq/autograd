package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.{Apply0, V}
import shapeless.Nat


sealed trait Op

// () -> z

trait NullaryOp extends Op {

  def deriv[N <: Nat](shape: Shape[N]): V[N] = Apply0(shape, ZeroOp)

}

// x -> z

trait UnaryOp extends Op {

  def deriv[N <: Nat](v: V[N]): V[N]

}


trait UnaryFoldOp extends Op {

  def deriv[N <: Nat](v: V[N]): V[N]

}


trait UnaryExpandOp extends Op {

  def deriv[N <: Nat](v: V[N]): V[N]

}



// (x, y) -> z

trait BinaryOp extends Op {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat])

}

trait BinaryFoldOp extends Op {

  def deriv[L <: Nat, R <: Nat](l: V[L], r: V[R]): (V[_ <: Nat], V[_ <: Nat])

}
