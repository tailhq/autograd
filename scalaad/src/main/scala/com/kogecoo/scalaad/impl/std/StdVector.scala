package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{S1, Shape1, Tensor1}


case class StdVector(data: StdVec[Double], transposed: Boolean=false) extends Tensor1 {

  def toStdFloat: StdVec[Float] = data.map(_.toFloat)
  def toStdDouble: StdVec[Double] = data

  override def shape: S1 = Shape1(data.size)

}
