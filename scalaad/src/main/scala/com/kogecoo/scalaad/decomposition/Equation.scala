package com.kogecoo.scalaad.decomposition

trait Equation[S <: Shape]

class Equation1[S <: Shape](left: Param[S], right: Param[S], op: UnaryOp[S, S]) extends Equation[S]
class Equation2[S <: Shape](left: Param[S], right1: Param[S], right2: Param[S], op: BinaryOp[S, S, S]) extends Equation[S]

class Elementwise1[S <: Shape](left: Param[S], right: Param[S], op: UnaryOp[S, S]) extends Equation[S]  // other words, Broadcast
class Elementwise2[S <: Shape](left: Param[S], right1: Param[S], right2: Param[S], op: BinaryOp[S, S, S]) extends Equation[S]

class Fold1[SO <: Shape, SI1 <: Shape](left: Param[SO], right: Param[SI1], op: UnaryOp[SO, SI1])
class Fold2[SO <: Shape, SI1 <: Shape, SI2 <: Shape](left: Param[SO], right1: Param[SI1], right2: Param[SI2], op: BinaryOp[SO, SI1, SI2])

case class Param[S <: Shape]()

