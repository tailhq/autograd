package scalaad.impl.breeze

import scalaad._
import scalaad.impl.std
import scalaad.impl.std.{StdBooleanScalar, StdScalar}


case class BreezeScalar(data: T0) extends Tensor {

  private[this] val underlying = StdScalar(data)

  def toStdFloat: std.Scalar[Float] = underlying.toStdFloat

  def toStdDouble: std.T0 = underlying.toStdDouble

  override def shape: Shape = underlying.shape
}


case class BreezeVector(data: T1) extends Tensor {

  def toStdFloat: std.Vec[Float] = toStdDouble.map(_.toFloat)

  def toStdDouble: std.T1 = data.toArray.toSeq

  override def shape: Shape = Shape(data.size)
}


case class BreezeMatrix(data: T2) extends Tensor {

  override def shape: Shape = Shape(data.rows, data.cols)

  def toStdFloat: std.Mat[Float] = toStdDouble.map(_.map(_.toFloat))

  def toStdDouble: std.T2 = {
    (0 until data.rows).map { r =>
      (0 until data.cols).map { c => data(r, c) }
    }
  }

}

case class BreezeTensor3(data: T3) extends Tensor {

  override def shape: Shape = Shape(
    data.size,
    if (data.size > 0) data(0).rows else 0,
    if (data.size > 0) data(0).cols else 0
  )

  def toStdFloat: std.Vec[std.Mat[Float]] = toStdDouble.map(_.map(_.map(_.toFloat)))

  def toStdDouble: std.Vec[std.T2] = {
    (0 until data.size).map { a =>
      (0 until data(0).rows).map { r =>
        (0 until data(0).cols).map { c => data(a)(r, c) }
      }
    }
  }

}


case class BreezeBooleanScalar(data: B0) extends BooleanTensor {

  private[this] val underlying = StdBooleanScalar(data)

  override def shape: Shape = underlying.shape

  def toStd: std.B0 = underlying.toStd

}


case class BreezeBooleanVector(data: B1) extends BooleanTensor {

  override def shape: Shape = Shape(data.size)

  def toStd: std.B1 = data.toArray

}


case class BreezeBooleanMatrix(data: B2) extends BooleanTensor {

  override def shape: Shape = Shape(data.rows, data.cols)

  def toStd: std.B2 = {
    (0 until data.rows).map { r =>
      (0 until data.cols).map { c => data(r, c) }
    }
  }
}


case class BreezeBooleanTensor3(data: B3) extends BooleanTensor {

  override def shape: Shape = Shape(
    data.size,
    if (data.size > 0) data(0).rows else 0,
    if (data.size > 0) data(0).cols else 0
  )

  def toStd: std.Vec[std.B2] = {
    (0 until data.size).map { a =>
      (0 until data(0).rows).map { r =>
        (0 until data(0).cols).map { c => data(a)(r, c) }
      }
    }
  }

}

