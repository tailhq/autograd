package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{NullaryOp, ZeroOp}
import shapeless.Nat
import shapeless.ops.nat.Sum


// Nullary Application

trait Application0[N <: Nat] extends ValueExpr[N]


case class Apply0[N <: Nat](shape: Shape[N], op: NullaryOp) extends Application0[N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    implicit def sum: Sum.Aux[N, W, O] = new Sum[N, W] { type Out = O }
    val newShape = shape.extend[W, O](wrt.shape)
    Apply0(newShape, ZeroOp)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = () // nop for constant node
}
