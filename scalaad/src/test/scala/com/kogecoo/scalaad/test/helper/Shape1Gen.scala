package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.Shape
import org.scalacheck.Gen


object Shape1Gen {

  type SizeC = Int => Boolean

  type ShapeC = Shape => Boolean


  private[this] def genSize(min: Int, max: Int, constraint: SizeC): Gen[Int] = {
    Gen.choose[Int](min, max) suchThat constraint
  }

  def apply(min: Int, max: Int, sizeConstraint: SizeC, shapeConstraint: ShapeC): Gen[Shape] = {
    for {
      size <- genSize(min, max, sizeConstraint)
      shape <- Gen.const(Shape(size)) suchThat shapeConstraint
    } yield shape
  }

  def apply(size: Int, shapeConstraint: ShapeC): Gen[Shape] = {
    for {
      size <- Gen.const[Int](size)
      shape <- Gen.const(Shape(size)) suchThat shapeConstraint
    } yield shape
  }

}

