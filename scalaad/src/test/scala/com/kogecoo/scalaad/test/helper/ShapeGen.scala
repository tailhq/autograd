package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.Shape
import org.scalacheck.Gen


object ShapeGen {

  type IntC = Int => Boolean

  type ShapeC = Shape => Boolean

  private[this] def genInt(min: Int, max: Int, constraint: IntC): Gen[Int] = {
    Gen.choose[Int](min, max) suchThat constraint
  }

  def apply(n: Int, sizeMin: Int, sizeMax: Int, sizeConstraint: IntC, shapeConstraint: ShapeC): Gen[Shape] = {
    val sizeGen = genInt(sizeMin, sizeMax, sizeConstraint)
    for {
      sizes <- Gen.listOfN(n, sizeGen)
      shape <- Gen.const(Shape(sizes:_*))
    } yield shape
  }

}
