package com.kogecoo.scalaad.test.helper.impl.std

import org.scalacheck.{Arbitrary, Gen}


object StdValueGen {

  type T = Double

  private[this] val noValueConstraint: T => Boolean = T => true

  def apply(min: T): Gen[T] = apply(Some(min), None, noValueConstraint)
  def apply(max: T)(implicit d: DummyImplicit): Gen[T] = apply(None, Some(max), noValueConstraint)
  def apply(min: T, max: T): Gen[T] = apply(Some(min), Some(max), noValueConstraint)

  def apply(maybeMin: Option[T], maybeMax: Option[T], constraint: T => Boolean): Gen[T] = {
    val gen = (maybeMin, maybeMax) match {
      case (Some(min), Some(max)) => Gen.choose(min,             max)
      case (None,      Some(max)) => Gen.choose(Double.MinValue, max)
      case (Some(min), None)      => Gen.choose(min,             Double.MaxValue)
      case _                      => Arbitrary.arbitrary[T]
    }
    for (v <- gen suchThat constraint) yield v
  }

}
