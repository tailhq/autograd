package scalaad.impl.std

import scalaad.graph.DType
import scalaad.{Shape, Tensor}


case class StdScalar[A, D <: DType](data: Scalar[A]) extends Tensor[D] {

  def shape: Shape = Shape()

  def toStd: Scalar[A] = data

}


case class StdVector[A, D <: DType](data: Vec[A]) extends Tensor[D] {

  def shape: Shape = Shape(data.size)

  def toStd: Vec[A] = data

}


case class StdMatrix[A, D <: DType](data: Mat[A]) extends Tensor[D] {

  def shape: Shape = Shape(data.size, data.head.size)

  def toStd: Mat[A] = data
}
