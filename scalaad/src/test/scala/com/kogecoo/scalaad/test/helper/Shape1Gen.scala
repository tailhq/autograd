package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.{Shape, Shape1}
import org.scalacheck.Gen
import shapeless.Nat
import shapeless.Nat._1





object Shape1Gen {

  type SizeC = Int => Boolean

  type ShapeC = Shape[_1] => Boolean


  private[this] def genSize(min: Int, max: Int, constraint: SizeC): Gen[Int] = {
    Gen.choose[Int](min, max) suchThat constraint
  }

  def apply(min: Int, max: Int, sizeConstraint: SizeC, shapeConstraint: ShapeC): Gen[Shape[_1]] = {
    for {
      size <- genSize(min, max, sizeConstraint)
      shape <- Gen.const(Shape1(size)) suchThat shapeConstraint
    } yield shape
  }

  def apply(size: Int, shapeConstraint: ShapeC): Gen[Shape[_1]] = {
    for {
      size <- Gen.const[Int](size)
      shape <- Gen.const(Shape1(size)) suchThat shapeConstraint
    } yield shape
  }

}

