package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.impl.std.StdMat
import com.kogecoo.scalaad.{S2, Shape2, Tensor2}
import org.nd4j.linalg.api.ndarray.INDArray


case class Nd4jMatrix(data: INDArray) extends Tensor2 {
  def toStdFloat: StdMat[Float] = toStdDouble.map(_.map(_.toFloat))
  def toStdDouble: StdMat[Double] = (0 until data.rows()).toArray.toSeq.map { i =>
    data.getRow(i).data().asDouble().toSeq
  }

  override def shape: S2 = Shape2(data.rows(), data.columns())
}
