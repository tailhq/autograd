package com.kogecoo.scalaad.test.helper

import com.kogecoo.scalaad.Shape
import org.scalacheck.Gen
import shapeless.Nat


// Experimental
object ShapeGen {

  type IntC = Int => Boolean

  type ShapeC[N <: Nat] = Shape[N] => Boolean

  private[this] def genInt(min: Int, max: Int, constraint: IntC): Gen[Int] = {
    Gen.choose[Int](min, max) suchThat constraint
  }

  def apply[N <: Nat](n: Int, sizeMin: Int, sizeMax: Int, sizeConstraint: IntC, shapeConstraint: ShapeC[N]): Gen[Shape[N]] =
  {
    val genSize = genInt(sizeMin, sizeMax, sizeConstraint)
    for {
      size  <- Gen.listOfN(n, genSize)
      shape <- Gen.const(Shape[N](size))
    } yield shape
  }

}
