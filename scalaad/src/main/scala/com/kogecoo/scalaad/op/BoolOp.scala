package com.kogecoo.scalaad.op


sealed trait BooleanOperator


trait UnaryBoolOp extends BooleanOperator

trait BinaryBoolOp extends BinaryBoolOp


trait BoolOp0 extends UnaryBoolOp

trait BoolOp1 extends UnaryBoolOp

trait BoolOp2 extends UnaryBoolOp

trait BoolOp00 extends BinaryBoolOp

trait BoolOp11 extends BinaryBoolOp

trait BoolOp22 extends BinaryBoolOp


