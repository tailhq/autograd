package com.kogecoo.scalaad.analyze

import com.kogecoo.scalaad.graph.bool.BooleanExpr
import com.kogecoo.scalaad.{S0, Shape}
import com.kogecoo.scalaad.op.{Add, BinaryOp, Cos, Div, Mul, Neg, NullaryOp, One, Op0, Op00, Sin, Tan, UnaryOp}


abstract class Equation[S <: Shape](val left: Param[S] = Param[S]())

case class Eqn0[S <: Shape](op: NullaryOp[S0]) extends Equation[S]

case class Eqn1[S <: Shape](right: Param[S], op: Op0) extends Equation[S] {

  def deriv(): Param[S] = {
    op match {
      case Sin => Eqn1(right, Cos).left
      case Cos => Eqn1(Eqn1(right, Sin).left, Neg).left
      case Tan => {
        val one = Eqn0(One).left
        val tan = Eqn1(right, Tan).left
        Eqn2(one, Eqn2(tan, tan, Mul).left, Add).left
      }
    }
  }
}

case class Eqn2[S <: Shape](right1: Param[S], right2: Param[S], op: Op00) extends Equation[S] {

  def deriv(): (Param[S], Param[S]) = {
    op match {
      case Add => (Eqn0(One).left, Eqn0(One).left)
      case Mul => (right2, right1)
      case Div => {
        val first = Eqn2(Eqn0(One).left, right2, Div)
        val second = Eqn2(right1, Eqn2(right2, right2, Mul).left, Div)
        (first.left, second.left)
      }
    }
  }
}


case class ElementwiseEqn1[SO <: Shape, SI1 <: Shape](right: Param[SI1], op: Op0) extends Equation[SO] { // other words, Broadcast

  def deriv(): Param[SO] = {
   op match {
      case Sin => ElementwiseEqn1(right, Cos).left
      case Cos => ElementwiseEqn1(Eqn1(right, Sin).left, Neg).left
      case Tan => {
        val one = Eqn0(One).left
        val tan = Eqn1(right, Tan).left
        ElementwiseLeftEqn2(one, Eqn2(tan, tan, Mul).left, Add).left
      }
    }
  }

}

case class ElementwiseLeftEqn2[SI1 <: Shape, SI2 <: Shape](right1: Param[SI1], right2: Param[SI2], op: Op00) extends Equation[SI1] {

  def deriv(): (Param[SI1], Param[SI1]) = {
    op match {
      case Add => (Eqn0(One).left, Eqn0(One).left)
      case Mul => (ElementwiseLeftEqn2(Eqn0[SI1](One).left, right2, Mul).left, right1)
      case Div => {
        val first = ElementwiseLeftEqn2(Eqn0[SI1](One).left, right2, Div)
        val second = ElementwiseLeftEqn2(right1, Eqn2(right2, right2, Mul).left, Div)
        (first.left, second.left)
      }
    }
  }

}

case class ElementwiseRightEqn2[SI1 <: Shape, SI2 <: Shape](right1: Param[SI1], right2: Param[SI2], op: Op00) extends Equation[SI2] {

  def deriv(): (Param[SI2], Param[SI2]) = {
    op match {
      case Add => (Eqn0(One).left, Eqn0(One).left)
      case Mul => (ElementwiseRightEqn2(Eqn0[SI1](One).left, right2, Mul).left, right1)
      case Div => {
        val first = ElementwiseRightEqn2(Eqn0[SI1](One).left, right2, Div)
        val second = ElementwiseRightEqn2(right1, Eqn2(right2, right2, Mul).left, Div)
        (first.left, second.left)
      }
    }
  }

}


case class FoldEqn1[SI1 <: Shape](right: Param[SI1], op: UnaryOp[S0, SI1]) extends Equation[S0] {

  def deriv(): Param[S0] = {
  }
}

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

