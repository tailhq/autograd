package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.V0


sealed trait Operator


trait UnaryOp extends Operator

trait BinaryOp extends Operator


trait Op0 extends UnaryOp { def apply(v: V0): V0 }

trait Op1 extends UnaryOp

trait Op2 extends UnaryOp

trait Op00 extends BinaryOp { def apply(l: V0, r: V0): V0 }

trait Op11 extends BinaryOp

trait Op22 extends BinaryOp
