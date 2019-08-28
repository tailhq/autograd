package io.github.tailabs.autograd.test.helper.rule

import scala.language.higherKinds


abstract class CompareRule[U[_], T] {

  def shouldBe(a: U[T], b: U[T])(implicit d: DummyImplicit): Boolean

  def shouldBe(a: T, b: T): Boolean

}
