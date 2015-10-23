package com.kogecoo.scalaad.test.helper.matcher

import com.kogecoo.scalaad.rule.Value
import com.kogecoo.scalaad.test.helper.rule.CompareRule

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
