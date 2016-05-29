package com.kogecoo.scalaad.analyze

import com.kogecoo.scalaad.{S0, Shape}
import com.kogecoo.scalaad.op.{BinaryOp, UnaryOp}


trait Equation[S <: Shape]


case class Eqn1[S <: Shape](left: Param[S], right: Param[S], op: UnaryOp[S, S]) extends Equation[S]

case class Eqn2[S <: Shape](left: Param[S], right1: Param[S], right2: Param[S], op: BinaryOp[S, S, S]) extends Equation[S]


case class ElementwiseEqn1[S <: Shape](left: Param[S], right: Param[S], op: UnaryOp[S, S]) extends Equation[S]  // other words, Broadcast

case class ElementwiseEqn2[S <: Shape](left: Param[S], right1: Param[S], right2: Param[S], op: BinaryOp[S, S, S]) extends Equation[S]


case class Fold1Eqn[SO <: Shape, SI1 <: Shape](left: Param[SO], right: Param[SI1], op: UnaryOp[SO, SI1])

case class FoldEqn2[SO <: Shape, SI1 <: Shape, SI2 <: Shape](left: Param[SO], right1: Param[SI1], right2: Param[SI2], op: BinaryOp[SO, SI1, SI2])


case class Fill1Eqn[SO <: Shape](left: Param[SO], right: Param[S0], op: UnaryOp[SO, S0])


case class Param[S <: Shape]()

