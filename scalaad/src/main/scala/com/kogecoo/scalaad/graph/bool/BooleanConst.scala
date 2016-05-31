package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.graph.{BE0, BE1, BE2}
import com.kogecoo.scalaad.{BooleanTensor0, BooleanTensor1, BooleanTensor2, S0, S1, S2, Shape0}


case class BooleanConst0(data: BooleanTensor0) extends BE0 { override val shape: S0 = Shape0() }

case class BooleanConst1(data: BooleanTensor1, shape: S1) extends BE1

case class BooleanConst2(data: BooleanTensor2, shape: S2) extends BE2
