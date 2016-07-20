package scalaad.impl.nd4j

import org.nd4j.linalg.api.ndarray.INDArray

import scalaad.graph.{Bool, DType, Real}
import scalaad.impl.std
import scalaad.impl.std.StdScalar
import scalaad.{Shape, Tensor}


class Nd4jTensor[D <: DType](val data: INDArray) extends Tensor[D] {

  def shape: Shape = Shape(data.shape():_*)

}


case class Nd4jScalar(data: T0) extends Tensor[Real] {

  private[this] val underlying = StdScalar[T0, Real](data)

  def shape: Shape = underlying.shape

  def toStd: std.Scalar[T0] = underlying.data

}


case class Nd4jVector(override val data: INDArray) extends Nd4jTensor[Real](data) {

  def toStd: std.Vec[T0] = data.data.asDouble()

}


case class Nd4jMatrix(override val data: INDArray) extends Nd4jTensor[Real](data) {

  def toStd: std.Mat[T0] = {
    (0 until data.rows()).toArray.toSeq.map { i =>
      data.getRow(i).data().asDouble().toSeq
    }
  }

}


case class Nd4jBooleanScalar(data: B0) extends Tensor[Bool] {

  private[this] val underlying = StdScalar[B0, Bool](data)

  def shape: Shape = underlying.shape

  def toStd: std.Scalar[B0] = underlying.toStd

}


case class Nd4jBooleanVector(override val data: INDArray) extends Nd4jTensor[Bool](data) {

  def toStd: std.Vec[B0] = data.data.asDouble.toSeq.map(_ != 0.0)

}


case class Nd4jBooleanMatrix(override val data: INDArray) extends Nd4jTensor[Bool](data) {

  def toStd: std.Mat[B0] ={
    (0 until data.rows()).toArray.toSeq.map { i =>
      data.getRow(i).data().asDouble().toSeq.map(_ != 0.0)
    }
  }

}

object Nd4jTensor {

  def apply(data: INDArray): Nd4jTensor[Real] = {
    data.shape().length match {
      case 1 => Nd4jVector(data)
      case 2 => Nd4jMatrix(data)
      case _ => new Nd4jTensor[Real](data)
    }
  }
}


object Nd4jBooleanTensor {
  def apply(data: INDArray): Nd4jTensor[Bool] = {
    data.shape().length match {
      case 1 => Nd4jBooleanVector(data)
      case 2 => Nd4jBooleanMatrix(data)
      case _ => new Nd4jTensor[Bool](data)
    }
  }
}

