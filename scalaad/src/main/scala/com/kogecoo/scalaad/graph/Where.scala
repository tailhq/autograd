package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{S0, S1, S2, Shape}


/**
  * (Experimental) if (cond) a else b
  *
  * @tparam S the order of Node cond, a and b
  */
trait WhereBase[S <: Shape, Cond <: Shape] extends ValueExpr[S] {

  override val shape: S = a.shape

  def a: ValueExpr[S]

  def b: ValueExpr[S]

}


// e.g. Where1_1([true, false, false], [1, 2, 3], [-4, -5, -6]) results [1, -5, -6]

case class Where0_0(cond: B0, a: V0, b: V0) extends WhereBase[S0, S0]

case class Where1_1(cond: B1, a: V1, b: V1) extends WhereBase[S1, S1]

case class Where2_2(cond: B2, a: V2, b: V2) extends WhereBase[S2, S2]


case class Where0_1(cond: B0, a: V1, b: V1) extends WhereBase[S1, S0]

case class Where0_2(cond: B0, a: V2, b: V2) extends WhereBase[S2, S0]

