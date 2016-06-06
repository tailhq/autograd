package com.kogecoo.scalaad.op.bool


sealed trait BooleanOperator

trait UnaryBooleanOp extends BooleanOperator

trait BinaryBooleanOp extends BooleanOperator

trait AsymmetricLeftBinaryBooleanOp extends BooleanOperator

trait AsymmetricRightBinaryBooleanOp extends BooleanOperator
