package io.github.tailabs.autograd.test.helper.matcher

import io.github.tailabs.autograd.test.helper.rule.CompareRule
import io.github.tailabs.autograd.value.Value

import scala.language.higherKinds


object ValueMatcherAssert {

  implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {

    def shouldBe(v: T)(implicit compare: CompareRule[U, T]): Unit = {
      assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
    }

    def shouldBe(v: U[T])(implicit d: DummyImplicit, compare: CompareRule[U, T]): Unit = {
      assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
    }

    def shouldBe(v: Value[U, T])(implicit compare: CompareRule[U, T]): Unit = {
      assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
    }
  }
}

object ValueMatcherDiagrammedAssert {

  import org.scalatest.DiagrammedAssertions._

  implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {

    def shouldBe(v: T)(implicit compare: CompareRule[U, T]): Unit = {
      assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
    }

    def shouldBe(v: U[T])(implicit d: DummyImplicit, compare: CompareRule[U, T]): Unit = {
      assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
    }

    def shouldBe(v: Value[U, T])(implicit compare: CompareRule[U, T]): Unit = {
      assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
    }
  }

}
