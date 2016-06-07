package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{NullaryOp, ZeroOp}
import shapeless.Nat
import shapeless.ops.nat.Sum


// Nullary Application

case class Apply0[N <: Nat](shape: Shape[N], op: NullaryOp) extends ValueExpr[N] {

  def _forward[W <: Nat, O <: Nat](wrt: VE[W]): VE[O] = {
    implicit def sum: Sum.Aux[N, W, O] = new Sum[N, W] { type Out = O }
    val newShape = shape.append[W, O](wrt.shape)
    Apply0(newShape, ZeroOp)
  }

}
