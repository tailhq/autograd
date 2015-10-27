package com.kogecoo.scalaad.breeze.test

import breeze.linalg.DenseMatrix
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherDiagrammedAssert._
import com.kogecoo.scalaad.breeze.test.helper.rule.DenseMatrixDoubleSoftCompareRule
import com.kogecoo.scalaad.breeze.BreezeRule.Implicits._
import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue}

import scala.util.Random
import org.scalatest.FunSuite


class DotDenseMatrixDoubleSuite extends FunSuite {

  implicit val rule = new DenseMatrixDoubleSoftCompareRule

  def randMatrix(row: Int, col: Int): DenseMatrix[Double] = {
    val a = Array.fill[Double](row * col)(Random.nextDouble())
    new DenseMatrix[Double](row, col, a)
  }

  def onesLike(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    DenseMatrix.ones[Double](m.rows, m.cols)
  }

  def zerosLike(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    DenseMatrix.zeros[Double](m.rows, m.cols)
  }

  test("dot - DenseMatrix[Double]") {
    val row1 = Random.nextInt(9) + 1
    val col1 = Random.nextInt(9) + 1
    val col2 = Random.nextInt(9) + 1

    val row2 = Random.nextInt(9) + 1
    val col3 = Random.nextInt(9) + 1

    val var_ij = Var(randMatrix(row1, col1))
    val var_jk = Var(randMatrix(col1, col2))
    val var_ik = Var(randMatrix(row1, col2))

    val cc_ij = ContainerConst[DenseMatrix, Double](randMatrix(row1, col1))
    val cc_jk = ContainerConst[DenseMatrix, Double](randMatrix(col1, col2))
    val cc_ik = ContainerConst[DenseMatrix, Double](randMatrix(row1, col2))

    val sc2 = ScalarConst[DenseMatrix, Double](2.0)
    val sc5 = ScalarConst[DenseMatrix, Double](5.0)

    val value = NonContainerValue[DenseMatrix, Double](15.0f)
    val cValue = ContainerValue[DenseMatrix, Double](randMatrix(row1, col1))

    // ScalarConst dot ScalarConst
    val a1 = dot(sc2, sc5)

    a1.apply()              shouldBe sc2.data * sc5.data
    a1.deriv(var_ij)        shouldBe zerosLike(var_ij.data)
    a1.deriv(var_jk)        shouldBe zerosLike(var_jk.data)
    a1.propagate(value)     shouldBe 0.0
    a1.propagate(cValue)    shouldBe zerosLike(cValue.data)

    // ScalarConst dot ContainerConst
    val a2 = dot(sc2, cc_jk)

    a2.apply()              shouldBe sc2.data * cc_jk.data
    a2.deriv(var_ij)        shouldBe zerosLike(var_ij.data)
    a2.deriv(var_jk)        shouldBe zerosLike(var_jk.data)
    //a2.propagate(value)     shouldBe zerosLike(cc_jk.data)
    //a2.propagate(cValue)    shouldBe zerosLike(cValue.data)
/*
    // ScalarConst dot Var
    val a3 = dot(sc2, var_jk)

    a3.apply()              shouldBe sc2.data * var_jk.data
    a3.deriv(var_ij)        shouldBe Seq(0f, 0f)
    a3.deriv(var_jk)        shouldBe Seq(0f, 0f)
    a3.propagate(value)     shouldBe Seq(0f, 0f)
    a3.propagate(cValue)    shouldBe Seq(0.0f, 0.0f)

    // ContainerConst dot ScalarConst
    val a4 = dot(cc_ij, sc5)

    a4.apply()              shouldBe cc_ij.data * sc5.data
    a4.deriv(var_ij)        shouldBe Seq(0f, 0f)
    a4.deriv(var_jk)        shouldBe Seq(0f, 0f)
    a4.propagate(value)     shouldBe Seq(0f, 0f)
    a4.propagate(cValue)    shouldBe Seq(0.0f, 0.0f)

    // ContainerConst dot ContainerConst
    val a5 = dot(cc_ij, cc_jk)

    a5.apply()              shouldBe cc_ij.data * cc_jk.data
    a5.deriv(var_ij)        shouldBe Seq(0f, 0f)
    a5.deriv(var_jk)        shouldBe Seq(0f, 0f)
    a5.propagate(value)     shouldBe Seq(0f, 0f)
    a5.propagate(cValue)    shouldBe Seq(0.0f, 0.0f)

    // ContainerConst dot Var
    val a6 = dot(cc_ij, var_jk)

    a6.apply()              shouldBe cc_ij.data * var_jk.data
    a6.deriv(var_ij)        shouldBe Seq(0f, 0f)
    a6.deriv(var_jk)        shouldBe Seq(0f, 0f)
    a6.propagate(value)     shouldBe Seq(0f, 0f)
    a6.propagate(cValue)    shouldBe Seq(0.0f, 0.0f)

    // Var dot ScalarConst
    val a7 = dot(var_ij, sc5)

    a7.apply()              shouldBe var_ij.data * sc5.data
    a7.deriv(var_ij)        shouldBe Seq(0f, 0f)
    a7.deriv(var_jk)        shouldBe Seq(0f, 0f)
    a7.propagate(value)     shouldBe Seq(0f, 0f)
    a7.propagate(cValue)    shouldBe Seq(0.0f, 0.0f)

    // Var dot ContainerConst
    val a8 = dot(var_ij, cc_jk)

    a8.apply()              shouldBe var_ij.data * cc_jk.data
    a8.deriv(var_ij)        shouldBe Seq(0f, 0f)
    a8.deriv(var_jk)        shouldBe Seq(0f, 0f)
    a8.propagate(value)     shouldBe Seq(0f, 0f)
    a8.propagate(cValue)    shouldBe Seq(0.0f, 0.0f)

    // Var dot Var
    val a9 = dot(var_ij, var_jk)

    a9.apply()              shouldBe var_ij.data * var_jk.data
    a9.deriv(var_ij)        shouldBe Seq(0f, 0f)
    a9.deriv(var_jk)        shouldBe Seq(0f, 0f)
    a9.propagate(value)     shouldBe Seq(0f, 0f)
    a9.propagate(cValue)    shouldBe Seq(0.0f, 0.0f)
*/
  }

}

