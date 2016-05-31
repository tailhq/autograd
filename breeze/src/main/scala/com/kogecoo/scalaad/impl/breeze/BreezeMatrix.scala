package com.kogecoo.scalaad.impl.breeze

import breeze.linalg.DenseMatrix
import com.kogecoo.scalaad.impl.std.StdMat
import com.kogecoo.scalaad.{S2, Shape2, Tensor2}


case class BreezeMatrix(data: DenseMatrix[Double]) extends Tensor2 {
  def toStdFloat: StdMat[Float] = toStdDouble.map(_.map(_.toFloat))
  def toStdDouble: StdMat[Double] = { // FIXME: maybe too inefficient
    (0 until data.rows).map { r =>
      (0 until data.cols).map { c => data(r, c) }
    }
  }

  override def shape: S2 = Shape2(data.rows, data.cols)
}
