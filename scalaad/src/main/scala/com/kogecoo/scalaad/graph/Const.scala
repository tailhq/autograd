package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad._
import shapeless.Nat
import shapeless.Nat.{_0, _1, _2}


trait ConstBase[N <: Nat] extends ValueExpr[N]

trait ConstBase0 extends ConstBase[_0] { override def shape: Shape[_0] = Shape0() }


case class Const0(data: Tensor0) extends ConstBase0

case class Const1(data: Tensor1, shape: Shape[_1]) extends ConstBase[_1]

case class Const2(data: Tensor2, shape: Shape[_2]) extends ConstBase[_2]


case class One0() extends ConstBase0

case class One1(shape: Shape[_1]) extends ConstBase[_1]

case class One2(shape: Shape[_2]) extends ConstBase[_2]

case class Eye2(shape: Shape[_2]) extends ConstBase[_2]


case class Zero0() extends ConstBase0

case class Zero1(shape: Shape[_1]) extends ConstBase[_1]

case class Zero2(shape: Shape[_2]) extends ConstBase[_2]


case class Half0() extends ConstBase0

case class Half1(shape: Shape[_1]) extends ConstBase[_1]

case class Half2(shape: Shape[_2]) extends ConstBase[_2]
