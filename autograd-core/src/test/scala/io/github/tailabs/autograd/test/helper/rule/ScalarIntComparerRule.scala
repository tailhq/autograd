package io.github.tailabs.autograd.test.helper.rule

import io.github.tailabs.autograd.graph.Scalar


class ScalarIntCompareRule extends CompareRule[Scalar, Int] {

  def shouldBe(a: Scalar[Int], b: Scalar[Int])(implicit d: DummyImplicit): Boolean = a.data == b.data

  def shouldBe(a: Int, b: Int): Boolean =  a == b

}

