package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{NullaryOp, ZeroOp}
import shapeless.Nat


// Nullary Application

trait Application0[N <: Nat] extends V[N]


case class Apply0[N <: Nat](shape: Shape[N], op: NullaryOp) extends Application0[N] {

  def _forward[W <: Nat](wrt: V[W]): V[N] = Apply0(shape, ZeroOp)

  def _reverse[G <: Nat](g: V[G]): Grad[G] = Grad.empty[G] // nop for constant node
}
