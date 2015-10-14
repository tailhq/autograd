package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.ValueMatcherProp._
import com.kogecoo.scalaad.gen._
import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.rule.ScalarIntValueRule.Implicits._
import com.kogecoo.scalaad.rule.SeqFloatValueRule.Implicits._

import org.scalacheck.{Properties, Prop}
import org.scalacheck.Prop.forAll

import scala.language.higherKinds


object ContainerConstSpec extends Properties("ContainerConst") {

  val genScalarIntType = new ContainerConstSpecGen[Scalar, Int](new ScalarIntNodeGen, new ScalarIntValueGen)

  val genSeqFloatType = new ContainerConstSpecGen[Seq, Float](new SeqFloatNodeGen, new SeqFloatValueGen)

  property("[Scalar, Int] - apply")                     = genScalarIntType.apply
  property("[Scalar, Int] - deriv w.r.t. self")         = genScalarIntType.derivSelf
  property("[Scalar, Int] - deriv w.r.t. unknown node") = genScalarIntType.deriv
  property("[Scalar, Int] - propagate value")           = genScalarIntType.propagate
  property("[Scalar, Int] - grad")                      = genScalarIntType.grad

  property("[Seq, Float]  - apply")                     = genSeqFloatType.apply
  property("[Seq, Float]  - deriv w.r.t. self")         = genSeqFloatType.derivSelf
  property("[Seq, Float]  - deriv w.r.t. unknown node") = genSeqFloatType.deriv
  property("[Seq, Float]  - propagate value")           = genSeqFloatType.propagate
  property("[Seq, Float]  - grad")                      = genSeqFloatType.grad

}


class ContainerConstSpecGen[U[_], T](nodes: GenNode[U, T], values: GenValue[U, T])(implicit rule: ValueRule[U, T]) {

  def apply: Prop = forAll(nodes.genContainerConst) {
    c => c.apply() shouldBe c.data
  }

  def derivSelf = forAll(nodes.genContainerConst) {
    c => c.deriv(c) shouldBe rule.zeroAdd
  }

  def deriv = forAll(nodes.genContainerConst, nodes.genNode) {
    (c, v) => c.deriv(v) shouldBe rule.zeroAdd
  }

  def propagate = forAll(nodes.genContainerConst, values.genValue) {
    (c, v) => c.propagate(v) shouldBe rule.zeroAdd
  }

  def grad = forAll(nodes.genContainerConst) {
    c => c.grad() shouldBe rule.zeroAdd
  }

}

