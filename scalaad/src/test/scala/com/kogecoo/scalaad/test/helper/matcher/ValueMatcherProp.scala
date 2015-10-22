package com.kogecoo.scalaad.test.helper.matcher

import com.kogecoo.scalaad.rule.{ContainerValue, NonContainerValue, Value}
import com.kogecoo.scalaad.test.helper.rule.CompareRule
import org.scalacheck.Prop
import org.scalacheck.Prop.BooleanOperators

import scala.language.higherKinds


object ValueMatcherProp {

  implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {

    def shouldBe(v: T)(implicit compare: CompareRule[U, T]): Prop = self match {
      case a: NonContainerValue[U, T] => compare.eq(a.data, v) :| s"${a.data} shouldBe ${v}"
      case _                          => false                 :| s"expects NonContainerValue but ${self}"
    }

    def shouldBe(v: U[T])(implicit d: DummyImplicit, compare: CompareRule[U, T]): Prop = self match {
      case a: ContainerValue[U, T] => compare.eq(a.data, v) :| s"${a.data} shouldBe ${v}"
      case _                       => false                 :| s"expects ContaienrValue but ${self}"
    }

    def shouldBe(v: Value[U, T])(implicit compare: CompareRule[U, T]): Prop = (self, v) match {
      case (a: ContainerValue[U, T],    b: ContainerValue[U, T])    => compare.eq(a.data, b.data) :| s"expects ${a.data} shouldBe ${b.data}"
      case (a: NonContainerValue[U, T], b: NonContainerValue[U, T]) => compare.eq(a.data, b.data) :| s"expects ${a.data} shouldBe ${b.data}"
      case _                                                        => false                      :| s"expects same Value type but ${self}, ${v}"
    }

    def closeTo(v: T)(implicit compare: CompareRule[U, T]): Prop = self match {
      case a: NonContainerValue[U, T] => compare.closeTo(a.data, v) :| s"${a.data} closeTo ${v}"
      case _                          => false                      :| s"expects NonContainerValue but ${self}"
    }

    def closeTo(v: U[T])(implicit d: DummyImplicit, compare: CompareRule[U, T]): Prop = self match {
      case a: ContainerValue[U, T] => compare.closeTo(a.data, v) :| s"${a.data} closeTo ${v}"
      case _                       => false                      :| s"expects ContaienrValue but ${self}"
    }

    def closeTo(v: Value[U, T])(implicit compare: CompareRule[U, T]): Prop = (self, v) match {
      case (a: ContainerValue[U, T],    b: ContainerValue[U, T])    => compare.closeTo(a.data, b.data) :| s"expects ${a.data} closeTo ${b.data}"
      case (a: NonContainerValue[U, T], b: NonContainerValue[U, T]) => compare.closeTo(a.data, b.data) :| s"expects ${a.data} closeTo ${b.data}"
      case _                                                        => false                           :| s"expects same Value type but ${self}, ${v}"
    }
  }

}
