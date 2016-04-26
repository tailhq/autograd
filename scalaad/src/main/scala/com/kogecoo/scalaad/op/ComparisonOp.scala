package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{B0, V0}


sealed trait ComparisonOperator

trait BinaryComparisonOp extends ComparisonOperator


trait ComparisonOp00 extends BinaryComparisonOp { def apply(l: V0, r: V0): B0 }

trait ComparisonOp11 extends BinaryComparisonOp

trait ComparisonOp22 extends BinaryComparisonOp


