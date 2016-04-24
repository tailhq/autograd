package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{Shape0, Tensor0, Tensor1, Tensor2}


trait VarNode0 extends V0 { override val shape: S0 = Shape0() }

trait VarNode1 extends V1

trait VarNode2 extends V2


case class Var0(data: Tensor0) extends VarNode0

case class Var1(data: Tensor1, shape: S1) extends VarNode1

case class Var2(data: Tensor2, shape: S2) extends VarNode2


// Experimental
// FIXME: make it to be immutable style
case class ArbVar0(name: String, var data: Option[Tensor0]) extends VarNode0 {
  override val shape: S0 = Shape0()
  def :=(t: Tensor0): Unit = { data = Some(t) }
}

case class ArbVar1(name: String, var data: Option[Tensor1], shape: S1) extends VarNode1 {
  def :=(t: Tensor1): Unit = { data = Some(t) }
}

case class ArbVar2(name: String, var data: Option[Tensor2], shape: S2) extends VarNode2 {
  def :=(t: Tensor2): Unit = { data = Some(t) }
}
