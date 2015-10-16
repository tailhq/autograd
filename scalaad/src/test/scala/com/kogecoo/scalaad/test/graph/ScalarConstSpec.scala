package com.kogecoo.scalaad.test.graph

import com.kogecoo.scalaad.graph.Scalar
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherProp
import com.kogecoo.scalaad.test.helper.rule.{SeqFloatValueRule, ScalarIntValueRule}
import ValueMatcherProp._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.rule.ValueRule
import ScalarIntValueRule.Implicits._
import SeqFloatValueRule.Implicits._

import org.scalacheck.{Properties, Prop}
import org.scalacheck.Prop.forAll

import scala.language.higherKinds


object ScalarConstSpec extends Properties("ScalarConst") {

  val genScalarIntType = new ScalarConstSpecGen[Scalar, Int](new ScalarIntNodeGen, new ScalarIntValueGen)

  val genSeqFloatType = new ScalarConstSpecGen[Seq, Float](new SeqFloatNodeGen, new SeqFloatValueGen)

  property("[Scalar, Int] - apply")                     = genScalarIntType.apply
  property("[Scalar, Int] - deriv w.r.t. self")         = genScalarIntType.derivSelf
  property("[Scalar, Int] - deriv w.r.t. unknown Node") = genScalarIntType.deriv
  property("[Scalar, Int] - propagate value")           = genScalarIntType.propagate
  property("[Scalar, Int] - grad")                      = genScalarIntType.grad

  property("[Seq, Float]  - apply")                     = genSeqFloatType.apply
  property("[Seq, Float]  - deriv w.r.t. self")         = genSeqFloatType.derivSelf
  property("[Seq, Float]  - deriv w.r.t. unknown Node") = genSeqFloatType.deriv
  property("[Seq, Float]  - propagate value")           = genSeqFloatType.propagate
  property("[Seq, Float]  - grad")                      = genSeqFloatType.grad

}


class ScalarConstSpecGen[U[_], T](nodes: GenNode[U, T], values: GenValue[U, T])(implicit rule: ValueRule[U, T]){

  def apply: Prop = forAll(nodes.genScalarConst) {
    c => c.apply() shouldBe c.data
  }

  def derivSelf = forAll(nodes.genScalarConst) {
    c => c.deriv(c) shouldBe rule.zero
  }

  def deriv = forAll(nodes.genScalarConst, nodes.genNode) {
    (c, v) => c.deriv(v) shouldBe rule.zero
  }

  def propagate = forAll(nodes.genScalarConst, values.genValue) {
    (c, v) => c.propagate(v) shouldBe rule.zero
  }

  def grad = forAll(nodes.genScalarConst) {
    c => c.grad() shouldBe rule.zero
  }

}
