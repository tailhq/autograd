package com.kogecoo.scalaad.op.bool


sealed trait ComparisonOperator

trait UnaryComparisonOp extends ComparisonOperator

trait BinaryComparisonOp extends ComparisonOperator

