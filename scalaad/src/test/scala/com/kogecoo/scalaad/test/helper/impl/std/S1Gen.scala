package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.Shape1
import com.kogecoo.scalaad.graph.S1
import org.scalacheck.Gen


object S1Gen {

  private[this] def genSize(constraint: Int => Boolean, sizeMin: Int, sizeMax: Int): Gen[Int] = {
    Gen.choose[Int](sizeMin, sizeMax) suchThat constraint
  }

  def apply(sizeConstraint: Int => Boolean, shapeConstraint: S1 => Boolean, sizeMin: Int, sizeMax: Int): Gen[S1] = {
    for {
      size <- genSize(sizeConstraint, sizeMin, sizeMax)
      //trans <- arbitrary[Boolean]
      shape <- Gen.const(Shape1(size)) suchThat shapeConstraint
    } yield shape
  }

  def apply(size: Int, shapeConstraint: S1 => Boolean): Gen[S1] = {
    for {
      size <- Gen.const[Int](size)
      //trans <- arbitrary[Boolean]
      shape <- Gen.const(Shape1(size)) suchThat shapeConstraint
    } yield shape
  }

}

