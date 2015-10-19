package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.rule.{Value, ContainerValue, NonContainerValue}
import org.scalacheck.Gen

import scala.language.higherKinds


abstract class GenValue[U[_], T] {

  lazy val defaultRestriction = (_: T) => true

  def genValue(restrict: T => Boolean = defaultRestriction): Gen[Value[U, T]]

  def genNonContainerValue(restrict: T => Boolean = defaultRestriction): Gen[NonContainerValue[U, T]]

  def genContainerValue(restrict: T => Boolean = defaultRestriction): Gen[ContainerValue[U, T]]

}
