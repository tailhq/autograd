package com.kogecoo.scalaad.test.helper.matcher

import com.kogecoo.scalaad.rule.{ContainerValue, NonContainerValue, Value}
import com.kogecoo.scalaad.test.helper.rule.CompareRule

import scala.language.higherKinds


object ValueMatcher {

  implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {
    def shouldBe(v: T)(implicit compare: CompareRule[U, T]): Boolean = {
      val cond = (x: T) => compare.eq(x, v)
      assumeNonContainerValue(self, cond)
    }

    def shouldBe(v: U[T])(implicit d: DummyImplicit, compare: CompareRule[U, T]): Boolean = {
      val cond = (x: U[T]) => compare.eq(x, v)
      assumeContainerValue(self, cond)
    }

    def shouldBe(v: Value[U, T])(implicit compare: CompareRule[U, T]): Boolean = {
      (self, v) match {
        case (a: ContainerValue[U, T], b: ContainerValue[U, T])       => compare.eq(a.data, b.data)
        case (a: NonContainerValue[U, T], b: NonContainerValue[U, T]) => compare.eq(a.data, b.data)
        case _ => false
      }
    }

    def closeTo(v: T)(implicit compare: CompareRule[U, T]): Boolean = {
      val cond = (x: T) => compare.closeTo(x, v)
      assumeNonContainerValue(self, cond)
    }

    def closeTo(v: U[T])(implicit d: DummyImplicit, compare: CompareRule[U, T]): Boolean = {
      val cond = (x: U[T]) => compare.closeTo(x, v)
      assumeContainerValue(self, cond)
    }

    def closeTo(v: Value[U, T])(implicit compare: CompareRule[U, T]): Boolean = {
      (self, v) match {
        case (a: ContainerValue[U, T], b: ContainerValue[U, T])       => compare.closeTo(a.data, b.data)
        case (a: NonContainerValue[U, T], b: NonContainerValue[U, T]) => compare.closeTo(a.data, b.data)
        case _ => false
      }
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
