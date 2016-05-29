package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.Shape


sealed trait Operator[SO <: Shape]

trait UnaryOp[SO <: Shape, SI <: Shape] extends Operator[SO]

trait BinaryOp[SO <: Shape, SI1 <: Shape, SI2 <: Shape] extends Operator[SO]
