package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{BoolTensor0, BoolTensor1, BoolTensor2, Shape0}


case class BooleanConst0(data: BoolTensor0) extends B0 { override val shape: S0 = Shape0() }

case class BooleanConst1(data: BoolTensor1, shape: S1) extends B1

case class BooleanConst2(data: BoolTensor2, shape: S2) extends B2
