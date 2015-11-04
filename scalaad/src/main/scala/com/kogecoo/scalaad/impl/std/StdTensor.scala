package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{BooleanTensor, Shape, Tensor}



case class StdScalar(data: T0) extends Tensor {

  def shape: Shape = Shape()

  def toStdFloat: Scalar[Float] = data.toFloat

  def toStdDouble: Scalar[Double] = data

  def toStd: Scalar[T0] = data

}


case class StdVector(data: T1) extends Tensor {

  def shape: Shape = Shape(data.size)

  def toStdFloat: Vec[Float] = data.map(_.toFloat)

  def toStdDouble: Vec[Double] = data

  def toStd: Vec[T0] = data

}


case class StdMatrix(data: T2) extends Tensor {

  def shape: Shape = Shape(data.size, data.head.size)

  def toStdFloat: Mat[Float] = data.map(_.map(_.toFloat))

  def toStdDouble: Mat[Double] = data

  def toStd: Mat[T0] = data
}


case class StdBooleanScalar(data: Boolean) extends BooleanTensor {

  def shape: Shape = Shape()

  def toStd: Scalar[Boolean] = data

}


case class StdBooleanVector(data: Vec[Boolean]) extends BooleanTensor {

  def shape: Shape = Shape(data.size)

  def toStd: Vec[Boolean] = data

}


case class StdBooleanMatrix(data: Mat[Boolean]) extends BooleanTensor {

  def shape: Shape = Shape(data.size, data.head.size)

  def toStd: Mat[Boolean] = data
}
