package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.impl.std.StdVec
import com.kogecoo.scalaad.{S1, Shape1, Tensor1}
import org.nd4j.linalg.api.ndarray.INDArray


case class Nd4jVector(data: INDArray, transposed: Boolean=false) extends Tensor1 {
  def toStdFloat: StdVec[Float] = data.data.asDouble().map(_.toFloat)
  def toStdDouble: StdVec[Double] = data.data.asDouble()

  override def shape: S1 = Shape1(data.shape()(0))
}
