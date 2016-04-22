package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.graph.B
import com.kogecoo.scalaad.{BooleanTensor, Shape}
import shapeless.Nat


case class BooleanConst[N <: Nat](v: BooleanTensor[N]) extends B[N] {

  def shape: Shape[N] = v.shape

}

