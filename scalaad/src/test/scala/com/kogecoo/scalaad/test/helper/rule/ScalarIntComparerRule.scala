package com.kogecoo.scalaad.test.helper.rule

import com.kogecoo.scalaad.graph.Scalar


class ScalarIntCompareRule extends CompareRule[Scalar, Int] {
  override def eq(a: Scalar[Int], b: Scalar[Int])(implicit d: DummyImplicit): Boolean = {
    a.data == b.data
  }

  override def eq(a: Int, b: Int): Boolean = a == b
}

object ScalarIntComparerRule {

  object Implicits {

    implicit val scalarIntCompareRule = new ScalarIntCompareRule

  }

}
