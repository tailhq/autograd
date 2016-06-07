package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.{Apply0, VE}
import shapeless.Nat


sealed trait Op


trait NullaryOp extends Op {

  def deriv[N <: Nat](shape: Shape[N]): VE[N] = Apply0(shape, ZeroOp)

}


trait UnaryOp extends Op {

  def deriv[N <: Nat](v: VE[N]): VE[N]

}


trait FoldUnaryOp[O <: Nat, I <: Nat] extends Op {

}


trait ExpandUnaryOp[O <: Nat, I <: Nat] extends Op {

}


trait BinaryOp extends Op {

  def deriv[L <: Nat, R <: Nat](l: VE[L], r: VE[R]): (VE[L], VE[R])

}

trait FoldBinaryOp[O <: Nat, I <: Nat] extends Op {

}
