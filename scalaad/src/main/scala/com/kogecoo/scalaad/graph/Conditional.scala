package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape


/**
  * (Experimental) if (cond) a else b
  * @tparam S the order of Node cond, a and b
  */
trait WhereBase[S <: Shape, Cond <: Shape] extends Node[S] {
  override val shape: S = a.shape
  def a: Node[S]
  def b: Node[S]
}

// e.g. Where1_1([true, false, false], [1, 2, 3], [-4, -5, -6]) results [1, -5, -6]
case class Where0_0(cond: B0, a: N0, b: N0) extends WhereBase[S0, S0]
case class Where1_1(cond: B1, a: N1, b: N1) extends WhereBase[S1, S1]
case class Where2_2(cond: B2, a: N2, b: N2) extends WhereBase[S2, S2]

case class Where0_1(cond: B0, a: N1, b: N1) extends WhereBase[S1, S0]
case class Where0_2(cond: B0, a: N2, b: N2) extends WhereBase[S2, S0]
case class Where1_2(cond: B1, a: N2, b: N2) extends WhereBase[S2, S1]

// Element-wise Eq
case class Eq00(l :N0, r: N0) extends Cond00
case class Eq11(l :N1, r: N1) extends Cond11
case class Eq22(l :N2, r: N2) extends Cond22

// Broadcast Eq
case class Eq01(l :N0, r: N1) extends Cond01
case class Eq10(l :N1, r: N0) extends Cond10
case class Eq02(l :N0, r: N2) extends Cond02
case class Eq20(l :N2, r: N0) extends Cond20
//case class Eq12(l :N1, r: N2) extends Cond12
//case class Eq21(l :N2, r: N1) extends Cond21

// Element-wise Neq
case class Neq00(l :N0, r: N0) extends Cond00
case class Neq11(l :N1, r: N1) extends Cond11
case class Neq22(l :N2, r: N2) extends Cond22

// Broadcast Neq
case class Neq01(l :N0, r: N1) extends Cond01
case class Neq10(l :N1, r: N0) extends Cond10
case class Neq02(l :N0, r: N2) extends Cond02
case class Neq20(l :N2, r: N0) extends Cond20
//case class Neq12(l :N1, r: N2) extends Cond12
//case class Neq21(l :N2, r: N1) extends Cond21

// Element-wise Lt
case class Lt00(l :N0, r: N0) extends Cond00
case class Lt11(l :N1, r: N1) extends Cond11
case class Lt22(l :N2, r: N2) extends Cond22

// Broadcast Lt
case class Lt01(l :N0, r: N1) extends Cond01
case class Lt10(l :N1, r: N0) extends Cond10
case class Lt02(l :N0, r: N2) extends Cond02
case class Lt20(l :N2, r: N0) extends Cond20
//case class Lt12(l :N1, r: N2) extends Cond12
//case class Lt21(l :N2, r: N1) extends Cond21

// Element-wise Lte
case class Lte00(l :N0, r: N0) extends Cond00
case class Lte11(l :N1, r: N1) extends Cond11
case class Lte22(l :N2, r: N2) extends Cond22

// Broadcast Lte
case class Lte01(l :N0, r: N1) extends Cond01
case class Lte10(l :N1, r: N0) extends Cond10
case class Lte02(l :N0, r: N2) extends Cond02
case class Lte20(l :N2, r: N0) extends Cond20
//case class Lte12(l :N1, r: N2) extends Cond12
//case class Lte21(l :N2, r: N1) extends Cond21

// Element-wise Gt
case class Gt00(l :N0, r: N0) extends Cond00
case class Gt11(l :N1, r: N1) extends Cond11
case class Gt22(l :N2, r: N2) extends Cond22

// Broadcast Gt
case class Gt01(l :N0, r: N1) extends Cond01
case class Gt10(l :N1, r: N0) extends Cond10
case class Gt02(l :N0, r: N2) extends Cond02
case class Gt20(l :N2, r: N0) extends Cond20
//case class Gt12(l :N1, r: N2) extends Cond12
//case class Gt21(l :N2, r: N1) extends Cond21

// Element-wise Gte
case class Gte00(l :N0, r: N0) extends Cond00
case class Gte11(l :N1, r: N1) extends Cond11
case class Gte22(l :N2, r: N2) extends Cond22

// Broadcast Gte
case class Gte01(l :N0, r: N1) extends Cond01
case class Gte10(l :N1, r: N0) extends Cond10
case class Gte02(l :N0, r: N2) extends Cond02
case class Gte20(l :N2, r: N0) extends Cond20
//case class Gte12(l :N1, r: N2) extends Cond12
//case class Gte21(l :N2, r: N1) extends Cond21

// Element-wise Not
case class Not0(v: B0) extends Cond0
case class Not1(v: B1) extends Cond1
case class Not2(v: B2) extends Cond2

// Element-wise And
case class And00(l :B0, r: B0) extends BinaryBoolOp[S0, S0, S0] { val shape = l.shape }
case class And11(l :B1, r: B1) extends BinaryBoolOp[S1, S1, S1] { val shape = l.shape }
case class And22(l :B2, r: B2) extends BinaryBoolOp[S2, S2, S2] { val shape = l.shape }

// Broadcast And
case class And01(l :B0, r: B1) extends BinaryBoolOp[S1, S0, S1] { val shape = r.shape }
case class And10(l :B1, r: B0) extends BinaryBoolOp[S1, S1, S0] { val shape = l.shape }
case class And02(l :B0, r: B2) extends BinaryBoolOp[S2, S0, S2] { val shape = r.shape }
case class And20(l :B2, r: B0) extends BinaryBoolOp[S2, S2, S0] { val shape = l.shape }
//case class And12(l :B1, r: B2) extends BinaryBoolOp[S2, S1, S2] { val shape = r.shape }
//case class And21(l :B2, r: B1) extends BinaryBoolOp[S2, S2, S1] { val shape = l.shape }

// Element-wise Or
case class Or00(l :B0, r: B0) extends BinaryBoolOp[S0, S0, S0] { val shape = l.shape }
case class Or11(l :B1, r: B1) extends BinaryBoolOp[S1, S1, S1] { val shape = l.shape }
case class Or22(l :B2, r: B2) extends BinaryBoolOp[S2, S2, S2] { val shape = l.shape }

// Broadcast Or
case class Or01(l :B0, r: B1) extends BinaryBoolOp[S1, S0, S1] { val shape = r.shape }
case class Or10(l :B1, r: B0) extends BinaryBoolOp[S1, S1, S0] { val shape = l.shape }
case class Or02(l :B0, r: B2) extends BinaryBoolOp[S2, S0, S2] { val shape = r.shape }
case class Or20(l :B2, r: B0) extends BinaryBoolOp[S2, S2, S0] { val shape = l.shape }
//case class Or12(l :B1, r: B2) extends BinaryBoolOp[S2, S1, S2] { val shape = r.shape }
//case class Or21(l :B2, r: B1) extends BinaryBoolOp[S2, S2, S1] { val shape = l.shape }
