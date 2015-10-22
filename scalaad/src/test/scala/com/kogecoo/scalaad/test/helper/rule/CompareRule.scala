package com.kogecoo.scalaad.test.helper.rule

import scala.language.higherKinds


trait CompareRule[U[_], T] {

  def compare(a: U[T], b: U[T])(implicit d: DummyImplicit): Boolean

  def compare(a: T, b: T): Boolean

}
