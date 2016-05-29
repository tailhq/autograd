package com.kogecoo.scalaad.impl.breeze

import breeze.linalg.DenseMatrix
import com.kogecoo.scalaad._
import com.kogecoo.scalaad.graph.S2
import com.kogecoo.scalaad.impl.std.StdMat


case class BreezeBooleanMatrix(data: DenseMatrix[Boolean]) extends BooleanTensor2 {

  override def shape: S2 = Shape2(data.rows, data.cols)

  override def toStd: StdMat[Boolean] = { // FIXME: maybe too inefficient
    (0 until data.rows).map { r =>
      (0 until data.cols).map { c => data(r, c) }
    }
  }}
