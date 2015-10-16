package com.kogecoo.scalaad.test.helper.matcher

import com.kogecoo.scalaad.rule.Value

import scala.language.higherKinds


object ValueMatcherAssert {

  implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {

    def shouldBe(v: T): Unit = assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))

    def shouldBe(v: U[T])(implicit d: DummyImplicit): Unit = assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))

    def shouldBe(v: Value[U, T]): Unit = assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
  }

  implicit class ValueOpsForTest2[U[_]](val self: Value[U, Float]) extends AnyVal {

    def shouldBeLike(v: Float, eps: Float = 1e-3f): Unit = {
      assert(ValueMatcher.ValueOpsForTest2(self).shouldBeLike(v, eps))
    }
  }

}

object ValueMatcherDiagrammedAssert {
  import org.scalatest.DiagrammedAssertions._

  implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {

    def shouldBe(v: T): Unit = assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))

    def shouldBe(v: U[T])(implicit d: DummyImplicit): Unit = assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))

    def shouldBe(v: Value[U, T]): Unit = assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
  }

  implicit class ValueOpsForTest2[U[_]](val self: Value[U, Float]) extends AnyVal {

    def shouldBeLike(v: Float, eps: Float = 1e-3f): Unit = {
      assert(ValueMatcher.ValueOpsForTest2(self).shouldBeLike(v, eps))
    }
  }

}
