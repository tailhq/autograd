package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{Shape, Shape0, Tensor}
import shapeless.Nat
import shapeless.Nat.{_0, _1, _2}


trait VarBase[N <: Nat] extends V[N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    Eye(shape.extend(wrt.shape))
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = Grad[G](this, adj)

}

case class Var[N <: Nat](data: Tensor[N]) extends VarBase[N] { def shape: Shape[N] = data.shape }


// Future Work
// class Placeholder[N <: Nat](name: String, override val shape: Shape[N]) extends VarBase[N]
