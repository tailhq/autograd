package com.kogecoo.scalaad.op.bool

import com.kogecoo.scalaad.Shape


sealed trait ComparisonOperator[SO <: Shape]

trait UnaryComparisonOp[SO <: Shape, SI <: Shape] extends ComparisonOperator[SO]

trait BinaryComparisonOp[SO <: Shape, SI1 <: Shape, SI2 <: Shape] extends ComparisonOperator[SO]

