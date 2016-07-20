package scalaad.impl.std

import scalaad.{Shape, Tensor}



case class StdScalar(data: T0) extends Tensor {

  def shape: Shape = Shape()

  def toStd: Scalar[T0] = data

}


case class StdVector(data: T1) extends Tensor {

  def shape: Shape = Shape(data.size)

  def toStd: Vec[T0] = data

}


case class StdMatrix(data: T2) extends Tensor {

  def shape: Shape = Shape(data.size, data.head.size)

  def toStd: Mat[T0] = data
}


case class StdBooleanScalar(data: B0) extends Tensor {

  def shape: Shape = Shape()

  def toStd: B0 = data

}


case class StdBooleanVector(data: B1) extends Tensor {

  def shape: Shape = Shape(data.size)

  def toStd: B1 = data

}


case class StdBooleanMatrix(data: B2) extends Tensor {

  def shape: Shape = Shape(data.size, data.head.size)

  def toStd: B2 = data
}
