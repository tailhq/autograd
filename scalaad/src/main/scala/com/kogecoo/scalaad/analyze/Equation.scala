package com.kogecoo.scalaad.analyze

import com.kogecoo.scalaad.{S0, Shape}
import com.kogecoo.scalaad.op.{BinaryOp, Op0, Op00, UnaryOp}


trait Equation[S <: Shape]


case class Eqn1[S <: Shape](left: Param[S], right: Param[S], op: Op0) extends Equation[S]

case class Eqn2[S <: Shape](left: Param[S], right1: Param[S], right2: Param[S], op: Op00) extends Equation[S]


case class ElementwiseEqn1[SO <: Shape, SI1 <: Shape](left: Param[SO], right: Param[SI1], op: Op0) extends Equation[SO]  // other words, Broadcast

case class ElementwiseEqn2[SO <: Shape, SI1 <: Shape, SI2 <: Shape](left: Param[SO], right1: Param[SI1], right2: Param[SI2], op: Op00) extends Equation[SO]


case class FoldEqn1[SI1 <: Shape](left: Param[S0], right: Param[SI1], op: UnaryOp[S0, SI1]) extends Equation[S0]

case class FoldEqn2[SI1 <: Shape, SI2 <: Shape](left: Param[S0], right1: Param[SI1], right2: Param[SI2], op: BinaryOp[S0, SI1, SI2]) extends Equation[S0]


case class FillEqn1[SO <: Shape](left: Param[SO], right: Param[S0], shape: SO) extends Equation[SO]


case class Param[S <: Shape]()

