package com.kogecoo.scalaad.test.helper.rule

import com.kogecoo.scalaad.graph.Scalar


class ScalarIntCompareRule extends CompareRule[Scalar, Int] {
  override def compare(a: Scalar[Int], b: Scalar[Int])(implicit d: DummyImplicit): Boolean = {
    a.data == b.data
  }

  override def compare(a: Int, b: Int): Boolean = a == b

  override def closeTo(a: Scalar[Int], b: Scalar[Int], eps: Int = 0)(implicit d: DummyImplicit): Boolean = {
    a == b
  }

  override def closeTo(a: Int, b: Int, eps: Int = 0): Boolean = {
    a == b
  }
}

object ScalarIntComparerRule {

  object Implicits {

    implicit val scalarIntCompareRule = new ScalarIntCompareRule

  }

}
