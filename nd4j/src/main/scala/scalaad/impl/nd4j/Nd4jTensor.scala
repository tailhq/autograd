package scalaad.impl.nd4j

import scalaad.impl.std
import scalaad.impl.std.{StdBooleanScalar, StdScalar}
import scalaad.{Shape, Tensor}
import org.nd4j.linalg.api.ndarray.INDArray


case class Nd4jScalar(data: T0) extends Tensor {

  private[this] val underlying = StdScalar(data)

  def shape: Shape = underlying.shape

  def toStd: std.Scalar[T0] = underlying.data

}


case class Nd4jVector(override val data: INDArray) extends Nd4jTensor(data) {

  def toStd: std.Vec[Double] = data.data.asDouble()

}


case class Nd4jMatrix(override val data: INDArray) extends Nd4jTensor(data) {

  def toStd: std.Mat[Double] = {
    (0 until data.rows()).toArray.toSeq.map { i =>
      data.getRow(i).data().asDouble().toSeq
    }
  }

}



class Nd4jTensor(val data: INDArray) extends Tensor {

  def shape: Shape = Shape(data.shape():_*)

}


object Nd4jTensor {

  def apply(data: INDArray): Nd4jTensor = {
    data.shape().length match {
      case 1 => Nd4jVector(data)
      case 2 => Nd4jMatrix(data)
      case _ => Nd4jTensor(data)
    }
  }

}


case class Nd4jBooleanScalar(data: Boolean) extends Tensor {

  private[this] val underlying = StdBooleanScalar(data)

  def shape: Shape = underlying.shape

  def toStd: std.Scalar[Boolean] = underlying.toStd

}


case class Nd4jBooleanVector(override val data: INDArray) extends Nd4jBooleanTensor(data) {

  def toStd: std.Vec[Boolean] = data.data.asDouble.toSeq.map(_ != 0.0)

}


case class Nd4jBooleanMatrix(override val data: INDArray) extends Nd4jBooleanTensor(data) {

  def toStd: std.Mat[Boolean] ={
    (0 until data.rows()).toArray.toSeq.map { i =>
      data.getRow(i).data().asDouble().toSeq.map(_ != 0.0)
    }
  }

}

class Nd4jBooleanTensor(val data: INDArray) extends Tensor {

  def shape: Shape = Shape(data.shape():_*)

}


object Nd4jBooleanTensor {

  def apply(data: INDArray): Nd4jBooleanTensor = {
    data.shape().length match {
      case 1 => Nd4jBooleanVector(data)
      case 2 => Nd4jBooleanMatrix(data)
      case _ => Nd4jBooleanTensor(data)
    }
  }

}

