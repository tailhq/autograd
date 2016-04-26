package com.kogecoo.scalaad.op


sealed trait BooleanOperator


trait UnaryBooleanOp extends BooleanOperator

trait BinaryBooleanOp extends BooleanOperator


trait BooleanOp0 extends UnaryBooleanOp

trait BooleanOp1 extends UnaryBooleanOp

trait BooleanOp2 extends UnaryBooleanOp

trait BooleanOp00 extends BinaryBooleanOp

trait BooleanOp11 extends BinaryBooleanOp

trait BooleanOp22 extends BinaryBooleanOp


