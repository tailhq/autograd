package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{Shape, Shape2, Tensor2}
import shapeless.Nat._2


case class StdMatrix(data: T2) extends Tensor2 {

  def shape: Shape[_2] = Shape2(data.size, data.head.size)

  def toStdFloat: Mat[Float] = data.map(_.map(_.toFloat))

  def toStdDouble: Mat[Double] = data

}
