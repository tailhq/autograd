package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.Shape2
import com.kogecoo.scalaad.graph.S2
import org.scalacheck.Gen


object S2Gen {

  private[this] val noShapeConstraint: S2 => Boolean = _ => true
  private[this] val defaultRowSizeMin = 1
  private[this] val defaultRowSizeMax = 10
  private[this] val defaultColSizeMin = 1
  private[this] val defaultColSizeMax = 10

  private[this] def genSize(sizeMin: Int, sizeMax: Int, constraint: Int => Boolean): Gen[Int] = {
    Gen.choose[Int](sizeMin, sizeMax) suchThat constraint
  }

  def apply(
      rowCons: Int => Boolean,
      columnCons: Int => Boolean,
      shapeConstraint: S2 => Boolean,
      sizeRowMin: Int,
      sizeRowMax: Int,
      sizeColMin: Int,
      sizeColMax: Int
  ): Gen[S2] = {
    for {
      row   <- genSize(sizeRowMin, sizeRowMax, rowCons)
      col   <- genSize(sizeColMin, sizeColMax, columnCons)
      shape <- Gen.const(Shape2(row, col)) suchThat shapeConstraint
    } yield shape
  }

  def apply(
      row: Int,
      columnCons: Int => Boolean,
      shapeConstraint: S2 => Boolean,
      sizeColMin: Int,
      sizeColMax: Int
  ): Gen[S2] = {
    for {
      row   <- Gen.const[Int](row)
      col   <- genSize(sizeColMin, sizeColMax, columnCons)
      shape <- Gen.const(Shape2(row, col)) suchThat shapeConstraint
    } yield shape
  }

  def apply(
     rowCons: Int => Boolean,
     column: Int,
     shapeCons: S2 => Boolean,
     sizeRowMin: Int,
     sizeRowMax: Int
  ): Gen[S2] = {
     for {
      row   <- genSize(sizeRowMin, sizeRowMax, rowCons)
      col   <- Gen.const[Int](column)
      shape <- Gen.const(Shape2(row, col)) suchThat shapeCons
    } yield shape
  }

  def apply(row: Int, column: Int, shapeConstraint: S2 => Boolean): Gen[S2] = {
    for {
      row   <- Gen.const[Int](row)
      col   <- Gen.const[Int](column)
      shape <- Gen.const(Shape2(row, col)) suchThat shapeConstraint
    } yield shape
  }

}

