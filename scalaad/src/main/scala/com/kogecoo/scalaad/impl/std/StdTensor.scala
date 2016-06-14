package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{Shape, Shape0, Shape1, Shape2, Tensor}
import shapeless.Nat.{_0, _1, _2}



case class StdScalar(data: T0) extends Tensor[_0] {

  def shape: Shape[_0] = Shape0()

  def toStdFloat: Scalar[Float] = data.toFloat

  def toStdDouble: Scalar[Double] = data

  def toStd: Scalar[T0] = data

}


case class StdVector(data: T1) extends Tensor[_1] {

  def shape: Shape[_1] = Shape1(data.size)

  def toStdFloat: Vec[Float] = data.map(_.toFloat)

  def toStdDouble: Vec[Double] = data

  def toStd: Vec[T0] = data

}


case class StdMatrix(data: T2) extends Tensor[_2] {

  def shape: Shape[_2] = Shape2(data.size, data.head.size)

  def toStdFloat: Mat[Float] = data.map(_.map(_.toFloat))

  def toStdDouble: Mat[Double] = data

  def toStd: Mat[T0] = data
}
