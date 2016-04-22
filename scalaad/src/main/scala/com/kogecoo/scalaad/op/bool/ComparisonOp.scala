package com.kogecoo.scalaad.op.bool


sealed trait ComparisonOp

trait UnaryComparisonOp extends ComparisonOp

trait BinaryComparisonOp extends ComparisonOp

