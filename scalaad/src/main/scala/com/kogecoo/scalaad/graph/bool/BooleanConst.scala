package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.graph.{BE0, BE1, BE2}
import com.kogecoo.scalaad.{BooleanTensor0, BooleanTensor1, BooleanTensor2, Shape, Shape0}
import shapeless.Nat.{_0, _1, _2}


case class BooleanConst0(data: BooleanTensor0) extends BE0 { override val shape: Shape[_0] = Shape0() }

case class BooleanConst1(data: BooleanTensor1, shape: Shape[_1]) extends BE1

case class BooleanConst2(data: BooleanTensor2, shape: Shape[_2]) extends BE2
