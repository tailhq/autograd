package com.kogecoo.scalaad.op


sealed trait Operator


trait UnaryOp extends Operator

trait BinaryOp extends Operator


trait Op0 extends UnaryOp

trait Op1 extends UnaryOp

trait Op2 extends UnaryOp

trait Op00 extends BinaryOp

trait Op11 extends BinaryOp

trait Op22 extends BinaryOp


