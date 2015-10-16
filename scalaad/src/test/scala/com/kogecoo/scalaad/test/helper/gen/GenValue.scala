package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.rule.{Value, ContainerValue, NonContainerValue}
import org.scalacheck.Gen

import scala.language.higherKinds


abstract class GenValue[U[_], T] {

  def genValue: Gen[Value[U, T]]

  def genNonContainerValue: Gen[NonContainerValue[U, T]]

  def genContainerValue: Gen[ContainerValue[U, T]]

}
