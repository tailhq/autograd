package scalaad.impl.breeze

import scalaad._
import scalaad.graph.{Bool, Real}
import scalaad.impl.std
import scalaad.impl.std.StdScalar


case class BreezeScalar(data: T0) extends Tensor[Real] {

  private[this] val underlying = StdScalar(data)

  def toStd: std.T0 = underlying.data

  override def shape: Shape = underlying.shape
}


case class BreezeVector(data: T1) extends Tensor[Real] {

  def toStd: std.T1 = data.toArray.toSeq

  override def shape: Shape = Shape(data.size)
}


case class BreezeMatrix(data: T2) extends Tensor[Real] {

  override def shape: Shape = Shape(data.rows, data.cols)

  def toStd: std.T2 = {
    (0 until data.rows).map { r =>
      (0 until data.cols).map { c => data(r, c) }
    }
  }

}

case class BreezeTensor3(data: T3) extends Tensor[Real] {

  override def shape: Shape = Shape(
    data.size,
    if (data.size > 0) data(0).rows else 0,
    if (data.size > 0) data(0).cols else 0
  )

  def toStd: std.Vec[std.T2] = {
    (0 until data.size).map { a =>
      (0 until data(0).rows).map { r =>
        (0 until data(0).cols).map { c => data(a)(r, c) }
      }
    }
  }

}


case class BreezeBooleanScalar(data: B0) extends Tensor[Bool] {

  private[this] val underlying = StdScalar[B0, Bool](data)

  override def shape: Shape = underlying.shape

  def toStd: std.B0 = underlying.toStd

}


case class BreezeBooleanVector(data: B1) extends Tensor[Bool] {

  override def shape: Shape = Shape(data.size)

  def toStd: std.B1 = data.toArray

}


case class BreezeBooleanMatrix(data: B2) extends Tensor[Bool] {

  override def shape: Shape = Shape(data.rows, data.cols)

  def toStd: std.B2 = {
    (0 until data.rows).map { r =>
      (0 until data.cols).map { c => data(r, c) }
    }
  }
}


case class BreezeBooleanTensor3(data: B3) extends Tensor[Bool] {

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

