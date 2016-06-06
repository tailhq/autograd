package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{Shape, Shape0, Tensor0, Tensor1, Tensor2}
import shapeless.Nat
import shapeless.Nat.{ _0, _1, _2}


trait VarBase[N <: Nat] extends ValueExpr[N]

trait VarBase0 extends VarBase[_0] { override val shape: Shape[_0] = Shape0() }


case class Var0(data: Tensor0) extends VarBase0

case class Var1(data: Tensor1, shape: Shape[_1]) extends VarBase[_1]

case class Var2(data: Tensor2, shape: Shape[_2]) extends VarBase[_2]


// Experimental
// FIXME: make it to be immutable style
case class ArbVar0(name: String, var data: Option[Tensor0]) extends VarBase0 {

  def :=(t: Tensor0): Unit = { data = Some(t) }

}


case class ArbVar1(name: String, var data: Option[Tensor1], shape: Shape[_1]) extends VarBase[_1] {

  def :=(t: Tensor1): Unit = { data = Some(t) }

}

case class ArbVar2(name: String, var data: Option[Tensor2], shape: Shape[_2]) extends VarBase[_2] {

  def :=(t: Tensor2): Unit = { data = Some(t) }

}
