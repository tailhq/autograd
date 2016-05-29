package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.graph.S1
import com.kogecoo.scalaad.impl.std.StdVec
import com.kogecoo.scalaad.{BooleanTensor1, Shape1}
import org.nd4j.linalg.api.ndarray.INDArray


case class Nd4jBooleanVector(data: INDArray) extends BooleanTensor1 {

  override def shape: S1 = Shape1(data.shape()(0))

  override def toStd: StdVec[Boolean] = data.data.asDouble.toSeq.map(_ != 0.0)

}
