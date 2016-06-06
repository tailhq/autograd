package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{Shape, Shape2, Tensor2}
import shapeless.Nat._2


case class StdMatrix(data: StdMat[Double]) extends Tensor2 {
  def toStdFloat: StdMat[Float] = data.map(_.map(_.toFloat))
  def toStdDouble: StdMat[Double] = data

  override def shape: Shape[_2] = Shape2(data.size, data(0).size)
}
