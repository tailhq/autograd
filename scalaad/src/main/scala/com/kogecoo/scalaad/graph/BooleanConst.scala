package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{BooleanTensor, Shape}


case class BooleanConst(v: BooleanTensor) extends Expr {

  def shape: Shape = v.shape

}

