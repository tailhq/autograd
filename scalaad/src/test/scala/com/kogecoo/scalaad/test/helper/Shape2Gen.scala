package scalaad.test.helper

import scalaad.Shape
import org.scalacheck.Gen


object Shape2Gen {

  private[this] val noShapeConstraint: Shape => Boolean = _ => true
  private[this] val defaultRowSizeMin = 1
  private[this] val defaultRowSizeMax = 10
  private[this] val defaultColSizeMin = 1
  private[this] val defaultColSizeMax = 10

  private[this] def genSize(sizeMin: Int, sizeMax: Int, constraint: Int => Boolean): Gen[Int] = {
    Gen.choose[Int](sizeMin, sizeMax) suchThat constraint
  }

  def apply(
      sizeRowMin: Int,
      sizeRowMax: Int,
      rowCons: Int => Boolean,
      sizeColMin: Int,
      sizeColMax: Int,
      columnCons: Int => Boolean,
      shapeConstraint: Shape => Boolean
  ): Gen[Shape] = {
    for {
      row   <- genSize(sizeRowMin, sizeRowMax, rowCons)
      col   <- genSize(sizeColMin, sizeColMax, columnCons)
      shape <- Gen.const(Shape(row, col)) suchThat shapeConstraint
    } yield shape
  }

  def apply(
      row: Int,
      sizeColMin: Int,
      sizeColMax: Int,
      columnCons: Int => Boolean,
      shapeConstraint: Shape => Boolean
  ): Gen[Shape] = {
    for {
      row   <- Gen.const[Int](row)
      col   <- genSize(sizeColMin, sizeColMax, columnCons)
      shape <- Gen.const(Shape(row, col)) suchThat shapeConstraint
    } yield shape
  }

  def apply(
     sizeRowMin: Int,
     sizeRowMax: Int,
     rowCons: Int => Boolean,
     column: Int,
     shapeCons: Shape => Boolean
  ): Gen[Shape] = {
     for {
      row   <- genSize(sizeRowMin, sizeRowMax, rowCons)
      col   <- Gen.const[Int](column)
      shape <- Gen.const(Shape(row, col)) suchThat shapeCons
    } yield shape
  }

  def apply(row: Int, column: Int, shapeConstraint: Shape => Boolean): Gen[Shape] = {
    for {
      row   <- Gen.const[Int](row)
      col   <- Gen.const[Int](column)
      shape <- Gen.const(Shape(row, col)) suchThat shapeConstraint
    } yield shape
  }

}

