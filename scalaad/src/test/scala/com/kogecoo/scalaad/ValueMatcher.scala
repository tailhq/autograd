package com.kogecoo.scalaad

import com.kogecoo.scalaad.rule.{ ContainerValue, NonContainerValue, Value }

import scala.language.higherKinds


object ValueMatcher {

  implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {
    def shouldBe(v: T): Boolean = assumeNonContainerValue(self, (_: T) == v)

    def shouldBe(v: U[T])(implicit d: DummyImplicit): Boolean = assumeContainerValue(self, (_: U[T]) == v)

    def shouldBe(v: Value[U, T]): Boolean = {
      (self, v) match {
        case (a: ContainerValue[U, T], b: ContainerValue[U, T]) => a.data == b.data
        case (a: NonContainerValue[U, T], b: NonContainerValue[U, T]) => a.data == b.data
        case _ => false
      }
    }
  }

  implicit class ValueOpsForTest2[U[_]](val self: Value[U, Float]) extends AnyVal {
    def shouldBeLike(v: Float, eps: Float = 1e-3f): Boolean = {
      val f = (x: Float) => v - eps <= x && v + eps >= x
      assumeNonContainerValue(self, f)
    }
  }

  private[this] def assumeContainerValue[U[_], T](value: Value[U, T], cond: U[T] => Boolean): Boolean = {
    value match {
      case v: ContainerValue[U, T] => cond(v.data)
      case v: NonContainerValue[U, T] => false
    }
  }

  private[this] def assumeNonContainerValue[U[_], T](value: Value[U, T], cond: T => Boolean): Boolean = {
    value match {
      case v: ContainerValue[U, T] => false
      case v: NonContainerValue[U, T] => cond(v.data)
    }
  }

}
