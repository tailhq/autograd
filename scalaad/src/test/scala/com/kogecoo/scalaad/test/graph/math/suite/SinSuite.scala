package com.kogecoo.scalaad.test.graph.math.suite

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.rule.{ContainerValue, NonContainerValue}
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherAssert._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatCompareRule.Implicits._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._

import scala.util.Random
import org.scalatest.FunSuite


class SinSuite extends FunSuite {

  test("Sin - Seq[Float]") {

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
      val scSin= sin(ccNode)

      scSin.apply()               closeTo ccSeq.map(scala.math.sin(_).toFloat)
      scSin.deriv(varNode)        closeTo Seq.fill(n)(0f)
      scSin.deriv(ccNode)         closeTo Seq.fill(n)(0f)
      scSin.deriv(scNode)         closeTo Seq.fill(n)(0f)
      scSin.propagate(valueRand)  closeTo Seq.fill(n)(0f)
      scSin.propagate(cValueRand) closeTo Seq.fill(n)(0f)

      // ContainerConst
      val ccSin = sin(ccNode)

      ccSin.apply()               closeTo ccSeq.map(scala.math.sin(_).toFloat)
      ccSin.deriv(varNode)        closeTo Seq.fill(n)(0f)
      ccSin.deriv(ccNode)         closeTo Seq.fill(n)(0f)
      ccSin.deriv(scNode)         closeTo Seq.fill(n)(0f)
      ccSin.propagate(valueRand)  closeTo Seq.fill(n)(0f)
      ccSin.propagate(cValueRand) closeTo Seq.fill(n)(0f)

      // Var
      val varSin = sin(varNode)

      varSin.apply()               closeTo varSeq.map(scala.math.sin(_).toFloat)
      varSin.deriv(varNode)        closeTo varSeq.map { v => (scala.math.cos(v)).toFloat }
      varSin.deriv(ccNode)         closeTo Seq.fill(n)(0f)
      varSin.deriv(scNode)         closeTo Seq.fill(n)(0f)
      varSin.propagate(valueRand)  closeTo varSeq.map { v => (value * scala.math.cos(v)).toFloat }
      varSin.propagate(cValueRand) closeTo varSeq.map({ v => scala.math.cos(v) }).zip(cValue).map { case (a, b) => (a * b).toFloat }

    }

  }

}
