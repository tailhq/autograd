package com.kogecoo.scalaad.test.graph.math.suite

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.rule.{ContainerValue, NonContainerValue}
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherAssert._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatCompareRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._

import scala.util.Random
import org.scalatest.FunSuite


class CosSuite extends FunSuite {

  test("Cos - Seq[Float]") {

    val n = 10
    val varSeq = Seq.fill[Float](n)(Random.nextFloat())
    val ccSeq = Seq.fill[Float](n)(Random.nextFloat())
    val sc = Random.nextFloat()
    val value = Random.nextFloat()
    val cValue = Seq.fill(n)(Random.nextFloat())

    val varNode = Var[Seq, Float](varSeq)
    val ccNode = ContainerConst[Seq, Float](ccSeq)
    val scNode = ScalarConst(sc)

    val valueRand= NonContainerValue[Seq, Float](value)
    val cValueRand = ContainerValue[Seq, Float](cValue)

    (0 to 100).foreach { i =>
      // ScalarConst
      val scCos= cos(ccNode)

      scCos.apply()               closeTo ccSeq.map(scala.math.cos(_).toFloat)
      scCos.deriv(varNode)        closeTo Seq.fill(n)(0f)
      scCos.deriv(ccNode)         closeTo Seq.fill(n)(0f)
      scCos.deriv(scNode)         closeTo Seq.fill(n)(0f)
      scCos.propagate(valueRand)  closeTo Seq.fill(n)(0f)
      scCos.propagate(cValueRand) closeTo Seq.fill(n)(0f)

      // ContainerConst
      val ccCos = cos(ccNode)

      ccCos.apply()               closeTo ccSeq.map(scala.math.cos(_).toFloat)
      ccCos.deriv(varNode)        closeTo Seq.fill(n)(0f)
      ccCos.deriv(ccNode)         closeTo Seq.fill(n)(0f)
      ccCos.deriv(scNode)         closeTo Seq.fill(n)(0f)
      ccCos.propagate(valueRand)  closeTo Seq.fill(n)(0f)
      ccCos.propagate(cValueRand) closeTo Seq.fill(n)(0f)

      // Var
      val varCos = cos(varNode)

      varCos.apply()               closeTo varSeq.map(scala.math.cos(_).toFloat)
      varCos.deriv(varNode)        closeTo varSeq.map { v => (-scala.math.sin(v)).toFloat }
      varCos.deriv(ccNode)         closeTo Seq.fill(n)(0f)
      varCos.deriv(scNode)         closeTo Seq.fill(n)(0f)
      varCos.propagate(valueRand)  closeTo varSeq.map { v => (-value * scala.math.sin(v)).toFloat }
      varCos.propagate(cValueRand) closeTo varSeq.map({ v => -scala.math.sin(v) }).zip(cValue).map { case (a, b) => (a * b).toFloat }

    }

  }

}
