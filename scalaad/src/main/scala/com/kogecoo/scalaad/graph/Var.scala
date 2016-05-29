package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{S0, S1, S2, Shape0, Tensor0, Tensor1, Tensor2}


trait VarBase0 extends V0 { override val shape: S0 = Shape0() }

trait VarBase1 extends V1

trait VarBase2 extends V2


case class Var0(data: Tensor0) extends VarBase0

case class Var1(data: Tensor1, shape: S1) extends VarBase1

case class Var2(data: Tensor2, shape: S2) extends VarBase2


// Experimental
// FIXME: make it to be immutable style
case class ArbVar0(name: String, var data: Option[Tensor0]) extends VarBase0 {

  val shape: S0 = Shape0()

  def :=(t: Tensor0): Unit = { data = Some(t) }

}


case class ArbVar1(name: String, var data: Option[Tensor1], shape: S1) extends VarBase1 {

  def :=(t: Tensor1): Unit = { data = Some(t) }

}

case class ArbVar2(name: String, var data: Option[Tensor2], shape: S2) extends VarBase2 {

  def :=(t: Tensor2): Unit = { data = Some(t) }

}
