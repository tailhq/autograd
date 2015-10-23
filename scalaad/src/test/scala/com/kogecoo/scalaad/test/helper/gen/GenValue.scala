package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.rule.{Value, ContainerValue, NonContainerValue}
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

import scala.language.higherKinds


abstract class GenValue[U[_], T] {

  lazy val defaultRestriction = (_: T) => true

  final def genValue(restrict: T => Boolean = defaultRestriction): Gen[Value[U, T]] = {
    oneOf(genNonContainerValue(restrict), genContainerValue(restrict))
  }

  final def genNonContainerValue(restrict: T => Boolean = defaultRestriction): Gen[NonContainerValue[U, T]] = {
    genNonContainerValueWithSource(restrict).map(_._1)
  }

  final def genContainerValue(restrict: T => Boolean = defaultRestriction): Gen[ContainerValue[U, T]] = {
    genContainerValueWithSource(restrict).map(_._1)
  }

  def genNonContainerValueWithSource(restrict: T => Boolean = defaultRestriction): Gen[(NonContainerValue[U, T], T)]

  def genContainerValueWithSource(restrict: T => Boolean = defaultRestriction): Gen[(ContainerValue[U, T], U[T])]

}
