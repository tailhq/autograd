package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.{Shape, Shape1}
import org.scalacheck.Gen
import shapeless.Nat._1


object S1Gen {

  private[this] def genSize(constraint: Int => Boolean, sizeMin: Int, sizeMax: Int): Gen[Int] = {
    Gen.choose[Int](sizeMin, sizeMax) suchThat constraint
  }

  def apply(sizeConstraint: Int => Boolean, shapeConstraint: Shape[_1] => Boolean, sizeMin: Int, sizeMax: Int): Gen[Shape[_1]] = {
    for {
      size <- genSize(sizeConstraint, sizeMin, sizeMax)
      //trans <- arbitrary[Boolean]
      shape <- Gen.const(Shape1(size)) suchThat shapeConstraint
    } yield shape
  }

  def apply(size: Int, shapeConstraint: Shape[_1] => Boolean): Gen[Shape[_1]] = {
    for {
      size <- Gen.const[Int](size)
      //trans <- arbitrary[Boolean]
      shape <- Gen.const(Shape1(size)) suchThat shapeConstraint
    } yield shape
  }

}

