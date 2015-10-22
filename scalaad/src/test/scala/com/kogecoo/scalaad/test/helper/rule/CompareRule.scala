package com.kogecoo.scalaad.test.helper.rule

import scala.language.higherKinds


trait CompareRule[U[_], T] {

  val defaultEPS: T

  def eq(a: U[T], b: U[T])(implicit d: DummyImplicit): Boolean

  def eq(a: T, b: T): Boolean

  final def closeTo(a: U[T], b: U[T])(implicit d: DummyImplicit): Boolean = closeTo(a, b, defaultEPS)

  final def closeTo(a: T, b: T): Boolean = closeTo(a, b, defaultEPS)

  def closeTo(a: U[T], b: U[T], eps: T)(implicit d: DummyImplicit): Boolean

  def closeTo(a: T, b: T, eps: T = defaultEPS): Boolean

}
