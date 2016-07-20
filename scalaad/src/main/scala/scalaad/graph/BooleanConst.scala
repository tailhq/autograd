package scalaad.graph

import scalaad.{BooleanTensor, Shape}


case class BooleanConst(v: BooleanTensor) extends Expr {

  def shape: Shape = v.shape

}

