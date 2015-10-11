package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.Node
import com.kogecoo.scalaad.rule.{ ContainerValue, NonContainerValue, Value }
import scala.language.higherKinds

object TestUtil {

  def assertContainerValue[U[_], T](value: Value[U, T], cond: U[T] => Boolean) {
    value match {
      case v: ContainerValue[U, T] => assert(cond(v.data))
      case v: NonContainerValue[U, T] => assert (false)
    }
  }

  def assertNonContainerValue[U[_], T](value: Value[U, T], cond: T => Boolean) {
    value match {
      case v: ContainerValue[U, T] => assert (false)
      case v: NonContainerValue[U, T] => assert(cond(v.data))
    }
  }

  implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {
    def shouldBe(v: T): Unit = assertNonContainerValue(self, (_: T) == v)

    def shouldBe(v: U[T])(implicit d: DummyImplicit): Unit = assertContainerValue(self, (_: U[T]) == v)


  }

  implicit class ValueOpsForTest2[U[_]](val self: Value[U, Float]) extends AnyVal {
    def shouldBeLike(v: Float, eps: Float=1e-3f): Unit = {
      val f = (x: Float) => v - eps <= x && v + eps >= x
      assertNonContainerValue(self, f)
    }
  }

}
