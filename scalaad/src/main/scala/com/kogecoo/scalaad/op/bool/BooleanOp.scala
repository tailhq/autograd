package com.kogecoo.scalaad.op.bool


sealed trait BooleanOp

trait UnaryBooleanOp extends BooleanOp

trait BinaryBooleanOp extends BooleanOp

trait UnaryBooleanExpandOp extends BooleanOp
