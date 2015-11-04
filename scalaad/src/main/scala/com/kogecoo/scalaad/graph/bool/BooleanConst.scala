package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.graph.B
import com.kogecoo.scalaad.{BooleanTensor, Shape}


case class BooleanConst(v: BooleanTensor) extends B {

  def shape: Shape = v.shape

}

