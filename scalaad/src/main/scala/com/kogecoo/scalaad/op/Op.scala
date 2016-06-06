package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.{Apply0, VE}
import shapeless.Nat


sealed trait Operator

trait NullaryOp extends Operator {

  def deriv[N <: Nat](shape: Shape[N]): VE[N] = Apply0(shape, Zero)

}

trait UnaryOp extends Operator {

  def deriv[N <: Nat](v: VE[N]): VE[N]

}

trait FoldUnaryOp[O <: Nat, I <: Nat] extends Operator {

}

trait ExpandUnaryOp[O <: Nat, I <: Nat] extends Operator {

}

trait BinaryOp extends Operator {

  def deriv[N <: Nat](l: VE[N], r: VE[N]): (VE[N], VE[N])

}

trait AsymmetricLeftBinaryOp extends Operator {

  def deriv[L <: Nat, R <: Nat](l: VE[L], r: VE[R]): (VE[L], VE[L])

}

trait AsymmetricRightBinaryOp extends Operator {

  def deriv[L <: Nat, R <: Nat](l: VE[L], r: VE[R]): (VE[R], VE[R])

}

trait FoldBinaryOp[O <: Nat, I <: Nat] extends Operator {

}
