package io.github.tailabs.autograd.test.graph

import io.github.tailabs.autograd.graph.Scalar
import io.github.tailabs.autograd.test.helper.gen._
import io.github.tailabs.autograd.test.helper.matcher.ValueMatcherProp._
import io.github.tailabs.autograd.test.helper.rule.{ScalarIntCompareRule, SeqFloatExactCompareRule, CompareRule}
import io.github.tailabs.autograd.test.helper.rule.ScalarIntValueRule.Implicits._
import io.github.tailabs.autograd.test.helper.rule.SeqFloatValueRule.Implicits._
import io.github.tailabs.autograd.rule.ValueRule

import org.scalacheck.{Properties, Prop}
import org.scalacheck.Prop.forAll

import scala.language.higherKinds


object ScalarConstSpec extends Properties("ScalarConst") {

  implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

  implicit val scalarIntCompareRule = new ScalarIntCompareRule

  val genScalarIntType = new ScalarConstSpecGen[Scalar, Int](new ScalarIntNodeGen, new ScalarIntValueGen)

  val genSeqFloatType = new ScalarConstSpecGen[Seq, Float](new SeqFloatNodeGen, new SeqFloatValueGen)

  property("[Scalar, Int] - apply")                    = genScalarIntType.apply
  property("[Scalar, Int] - deriv w.r.t. unknown var") = genScalarIntType.derivUnknownVar
  property("[Scalar, Int] - propagate value")          = genScalarIntType.propagate
  property("[Scalar, Int] - grad")                     = genScalarIntType.grad

  property("[Seq, Float]  - apply")                    = genSeqFloatType.apply
  property("[Seq, Float]  - deriv w.r.t. unknown var") = genSeqFloatType.derivUnknownVar
  property("[Seq, Float]  - propagate value")          = genSeqFloatType.propagate
  property("[Seq, Float]  - grad")                     = genSeqFloatType.grad

}


class ScalarConstSpecGen[U[_], T](nodes: GenNode[U, T], values: GenValue[U, T])(implicit rule: ValueRule[U, T], compare: CompareRule[U, T]){

  def apply: Prop = forAll(nodes.genScalarConst()) {
    c => c.apply() shouldBe c.data
  }

  def derivUnknownVar = forAll(nodes.genScalarConst(), nodes.genVar()) {
    (c, v) => c.deriv(v) shouldBe rule.zero(v())
  }

  def propagate = forAll(nodes.genScalarConst(), values.genValue()) {
    (c, v) => c.propagate(v) shouldBe rule.zero(v) * v
  }

  def grad = forAll(nodes.genScalarConst()) {
    c => c.grad() shouldBe rule.zero
  }

}
