package com.kogecoo.scalaad.test.helper.rule

import scala.language.higherKinds


trait CompareRule[U[_], T] {

  def eq(a: U[T], b: U[T])(implicit d: DummyImplicit): Boolean

  def eq(a: T, b: T): Boolean

}
