package com.kogecoo.scalaad.test.helper.rule

import com.kogecoo.scalaad.graph.Scalar


class ScalarIntCompareRule extends CompareRule[Scalar, Int] {

  override val defaultEPS = 0

  override def eq(a: Scalar[Int], b: Scalar[Int])(implicit d: DummyImplicit): Boolean = {
    a.data == b.data
  }

  override def eq(a: Int, b: Int): Boolean = a == b

  override def closeTo(a: Scalar[Int], b: Scalar[Int], eps: Int)(implicit d: DummyImplicit): Boolean = {
    scala.math.abs((a.data - b.data).toDouble) <= eps
  }

  override def closeTo(a: Int, b: Int, eps: Int): Boolean = {
    scala.math.abs((a - b).toDouble) <= eps
  }
}

object ScalarIntComparerRule {

  object Implicits {

    implicit val scalarIntCompareRule = new ScalarIntCompareRule

  }

}
