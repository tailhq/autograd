package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.analyze.{Analyzing, Param}
import com.kogecoo.scalaad.{S0, S1, S2, Shape, Shape0, Tensor0, Tensor1, Tensor2}


trait VarBase[S <: Shape] extends ValueExpr[S] {

  def analyze(a: Analyzing): Param[S] = a.addParam(this)

}

trait VarBase0 extends VarBase[S0] { override val shape: S0 = Shape0() }


case class Var0(data: Tensor0) extends VarBase0

case class Var1(data: Tensor1, shape: S1) extends VarBase[S1]

case class Var2(data: Tensor2, shape: S2) extends VarBase[S2]


// Experimental
// FIXME: make it to be immutable style
case class ArbVar0(name: String, var data: Option[Tensor0]) extends VarBase0 {

  def :=(t: Tensor0): Unit = { data = Some(t) }

}


case class ArbVar1(name: String, var data: Option[Tensor1], shape: S1) extends VarBase[S1] {

  def :=(t: Tensor1): Unit = { data = Some(t) }

}

case class ArbVar2(name: String, var data: Option[Tensor2], shape: S2) extends VarBase[S2] {

  def :=(t: Tensor2): Unit = { data = Some(t) }

}
