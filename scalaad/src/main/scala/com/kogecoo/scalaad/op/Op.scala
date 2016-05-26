package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.Shape


sealed trait Operator[SO <: Shape]


trait UnaryOp[SO <: Shape, SI1 <: Shape] extends Operator[SO]

trait BinaryOp[SO <: Shape, SI1 <: Shape, SI2 <: Shape] extends Operator[SO]


trait Op0 extends UnaryOp

trait Op1 extends UnaryOp

trait Op2 extends UnaryOp

trait Op00 extends BinaryOp

trait Op11 extends BinaryOp

trait Op22 extends BinaryOp


