package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{Shape, Shape0, Tensor}
import shapeless.Nat
import shapeless.Nat.{_0, _1, _2}


trait VarBase[N <: Nat] extends ValueExpr[N] {

  def _forward[W <: Nat, O <: Nat](wrt: ValueExpr[W]): ValueExpr[O] = {
    // FIXME: wrong
    One(shape.extend(wrt.shape))
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = {
    builder += ((this, g))
  }
}

class Var[N <: Nat](data: Tensor[N]) extends VarBase[N] { def shape: Shape[N] = data.shape }

object Var { def apply[N <: Nat](data: Tensor[N]): Var[N] = new Var[N](data) }

case class Var0(data: Tensor[_0]) extends VarBase[_0] { def shape: Shape[_0] = data.shape }

case class Var1(data: Tensor[_1]) extends VarBase[_1] { def shape: Shape[_1] = data.shape}

case class Var2(data: Tensor[_2]) extends VarBase[_2] { def shape: Shape[_2] = data.shape }


// Experimental
class ArbVar[N <: Nat](name: String, override val shape: Shape[N]) extends VarBase[N] {

  var data: Option[Tensor[N]] = None

  def :=(t: Tensor[N]): Unit = { data = Some(t) }

}

// FIXME: make it to be immutable style
case class ArbVar0(name: String) extends ArbVar[_0](name, Shape0())

case class ArbVar1(name: String, override val shape: Shape[_1]) extends ArbVar[_1](name, shape)

case class ArbVar2(name: String, override val shape: Shape[_2]) extends ArbVar[_2](name, shape)
