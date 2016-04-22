package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.impl.std.StdMat
import com.kogecoo.scalaad.{BooleanTensor2, S2, Shape2}
import org.nd4j.linalg.api.ndarray.INDArray


case class Nd4jBooleanMatrix(data: INDArray) extends BooleanTensor2 {

  override def toStd: StdMat[Boolean] ={
    (0 until data.rows()).toArray.toSeq.map { i =>
      data.getRow(i).data().asDouble().toSeq.map(_ != 0.0)
    }
  }

  override def shape: S2 = Shape2(data.rows(), data.columns())

}
