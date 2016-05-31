package com.kogecoo.scalaad.op.bool

import com.kogecoo.scalaad.Shape


sealed trait BooleanOperator[S <: Shape]

trait UnaryBooleanOp[SO <: Shape, SI <: Shape] extends BooleanOperator[SO]

trait BinaryBooleanOp[SO <: Shape, SI1 <: Shape, SI2 <: Shape] extends BooleanOperator[SO]
