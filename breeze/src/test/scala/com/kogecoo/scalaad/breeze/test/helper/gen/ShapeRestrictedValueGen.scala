package com.kogecoo.scalaad.breeze.test.helper.gen

import com.kogecoo.scalaad.test.helper.gen._
import org.scalacheck.Gen

import scala.language.higherKinds


abstract class ShapeRestrictedValueGen[U[_], T] {

  def genNonContainerValueWithSource(restrict: T => Boolean): Gen[NonContainerValueSample[U, T]]

  def genContainerValueWithSource(shape: MatrixShape, restrict: T => Boolean): Gen[ContainerValueSample[U, T]]

}