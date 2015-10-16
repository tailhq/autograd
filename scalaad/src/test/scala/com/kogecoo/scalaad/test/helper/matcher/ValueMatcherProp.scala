package com.kogecoo.scalaad.test.helper.matcher

import com.kogecoo.scalaad.rule.{ContainerValue, NonContainerValue, Value}
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators

import scala.language.higherKinds


object ValueMatcherProp {

  implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {
    def shouldBe(v: T): Prop = self match {
      case a: NonContainerValue[U, T] => (a.data == v) :| s"expects ${a.data} == ${v}"
      case _                          => false         :| s"expects NonContainerValue but ${self}"
    }

    def shouldBe(v: U[T])(implicit d: DummyImplicit): Prop = self match {
      case a: ContainerValue[U, T] => (a.data == v) :| s"expects ${a.data} == ${v}"
      case _                       => false         :| s"expects ContaienrValue but ${self}"
    }

    def shouldBe(v: Value[U, T]): Prop = (self, v) match {
      case (a: ContainerValue[U, T],    b: ContainerValue[U, T])    => (a.data == b.data) :| s"expects ${a.data} == ${b.data}"
      case (a: NonContainerValue[U, T], b: NonContainerValue[U, T]) => (a.data == b.data) :| s"expects ${a.data} == ${b.data}"
      case _                                                        => false              :| s"expects same Value type but ${self}, ${v}"
    }
  }

  implicit class ValueOpsForTest2[U[_]](val self: Value[U, Float]) extends AnyVal {
    def shouldBeLike(v: Float, eps: Float = 1e-3f): Prop = {
      val f = (x: Float) => v - eps <= x && v + eps >= x
      self match {
        case a: NonContainerValue[U, Float] => f(a.data) :| s"expects ${v - eps} <= ${a.data} <= ${v + eps}"
        case _                              => false     :| s"expects NonContainerValue but ${self}"
      }
    }
  }

}
