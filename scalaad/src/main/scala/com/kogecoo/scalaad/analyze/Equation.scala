package com.kogecoo.scalaad.analyze

import com.kogecoo.scalaad.graph.bool.BooleanExpr
import com.kogecoo.scalaad.{S0, Shape}
import com.kogecoo.scalaad.op.{BinaryOp, Op0, Op00, UnaryOp}


abstract class Equation[S <: Shape](val left: Param[S] = Param[S]())


case class Eqn1[S <: Shape](right: Param[S], op: Op0) extends Equation[S]

case class Eqn2[S <: Shape](right1: Param[S], right2: Param[S], op: Op00) extends Equation[S]


case class ElementwiseEqn1[SO <: Shape, SI1 <: Shape](right: Param[SI1], op: Op0) extends Equation[SO]  // other words, Broadcast

case class ElementwiseEqn2[SO <: Shape, SI1 <: Shape, SI2 <: Shape](right1: Param[SI1], right2: Param[SI2], op: Op00) extends Equation[SO]


case class FoldEqn1[SI1 <: Shape](right: Param[SI1], op: UnaryOp[S0, SI1]) extends Equation[S0]

case class FoldEqn2[SI1 <: Shape, SI2 <: Shape](right1: Param[SI1], right2: Param[SI2], op: BinaryOp[S0, SI1, SI2]) extends Equation[S0]


case class FillEqn1[SO <: Shape](right: Param[S0], shape: SO) extends Equation[SO]


case class WhereEqn[S <: Shape](cond: BooleanExpr[S], right1: Param[S], right2: Param[S]) extends Equation[S]


class Param[S <: Shape]() {

  def id: Int = System.identityHashCode(this)

  override def toString: String = s"Param(id=$id)"
}


object Param {

  def apply[S <: Shape](): Param[S] = new Param[S]

}

