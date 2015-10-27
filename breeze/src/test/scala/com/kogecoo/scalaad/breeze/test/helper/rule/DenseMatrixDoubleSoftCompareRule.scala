package com.kogecoo.scalaad.breeze.test.helper.rule

import breeze.linalg.DenseMatrix
import com.kogecoo.scalaad.test.helper.rule.CompareRule


class DenseMatrixDoubleSoftCompareRule extends CompareRule[DenseMatrix, Double] {

  val eps = (x: Double) => scala.math.abs(x) * 1e-3f

  def shouldBe(a: DenseMatrix[Double], b: DenseMatrix[Double])(implicit d: DummyImplicit): Boolean = {
    a.rows == b.rows && a.cols == b.cols &&
    (0 until a.rows).flatMap({ r =>
      (0 until a.cols).map { c => (a(r, c), b(r, c)) }
    }).forall { case (aelm, belm) =>
      aelm == belm || scala.math.abs(aelm - belm) <= eps(aelm) || (aelm.equals(Double.NaN) && belm.equals(Double.NaN))
    }
  }

  def shouldBe(a: Double, b: Double): Boolean = {
    a == b || scala.math.abs(a - b) <= eps(a) || (a.equals(Double.NaN) && b.equals(Double.NaN))
  }

}
