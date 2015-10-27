package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.value.ContainerValue
import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue, Value}
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

import scala.language.higherKinds


abstract class GenValue[U[_], T] {

  lazy val defaultRestriction = (_: T) => true

  final def genValue(restrict: T => Boolean = defaultRestriction): Gen[Value[U, T]] = {
    oneOf(genNonContainerValue(restrict), genContainerValue(restrict))
  }

  final def genNonContainerValue(restrict: T => Boolean = defaultRestriction): Gen[NonContainerValue[U, T]] = {
    genNonContainerValueWithSource(restrict).map(_.value)
  }

  final def genContainerValue(restrict: T => Boolean = defaultRestriction): Gen[ContainerValue[U, T]] = {
    genContainerValueWithSource(restrict).map(_.value)
  }

  def genNonContainerValueWithSource(restrict: T => Boolean = defaultRestriction): Gen[NonContainerValueSample[U, T]]

  def genContainerValueWithSource(restrict: T => Boolean = defaultRestriction): Gen[ContainerValueSample[U, T]]

}

