package io.github.tailabs.autograd.test.graph

import io.github.tailabs.autograd.graph.Scalar
import io.github.tailabs.autograd.test.helper.gen._
import io.github.tailabs.autograd.test.helper.matcher.ValueMatcherProp._
import io.github.tailabs.autograd.test.helper.rule.{SeqFloatExactCompareRule, ScalarIntCompareRule, CompareRule}
import io.github.tailabs.autograd.test.helper.rule.ScalarIntValueRule.Implicits._
import io.github.tailabs.autograd.test.helper.rule.SeqFloatValueRule.Implicits._
import io.github.tailabs.autograd.rule.ValueRule

import org.scalacheck.{Properties, Prop}
import org.scalacheck.Prop.forAll

import scala.language.higherKinds


object VarSpec extends Properties("Var") {

  implicit val scalarIntCompareRule = new ScalarIntCompareRule

  implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

  val genScalarIntType = new VarSpecGen[Scalar, Int](new ScalarIntNodeGen, new ScalarIntValueGen)

  val genSeqFloatType = new VarSpecGen[Seq, Float](new SeqFloatNodeGen, new SeqFloatValueGen)

  property("[Scalar, Int] - apply")                    = genScalarIntType.apply
  property("[Scalar, Int] - deriv w.r.t. self")        = genScalarIntType.derivSelf
  property("[Scalar, Int] - deriv w.r.t. unknown Var") = genScalarIntType.derivUnknownVar
  property("[Scalar, Int] - propagate value")          = genScalarIntType.propagate
  property("[Scalar, Int] - grad")                     = genScalarIntType.grad

  property("[Seq, Float]  - apply")                    = genSeqFloatType.apply
  property("[Seq, Float]  - deriv w.r.t. self")        = genSeqFloatType.derivSelf
  property("[Seq, Float]  - deriv w.r.t. unknown Var") = genSeqFloatType.derivUnknownVar
  property("[Seq, Float]  - propagate value")          = genSeqFloatType.propagate
  property("[Seq, Float]  - grad")                     = genSeqFloatType.grad

}


class VarSpecGen[U[_], T](nodes: GenNode[U, T], values: GenValue[U, T])(implicit rule: ValueRule[U, T], compare: CompareRule[U, T]) {

  def apply: Prop = forAll(nodes.genVar()) {
    c => c.apply () shouldBe c.data
  }

  def derivSelf = forAll(nodes.genVar()) {
    c => c.deriv(c) shouldBe rule.one(c())
  }

  def derivUnknownVar = forAll(nodes.genVar(), nodes.genVar()) {
    (c, v) => c.deriv(v) shouldBe rule.zero(v())
  }

  def propagate = forAll(nodes.genVar(), values.genValue()) {
    (c, v) => c.propagate(v) shouldBe rule.one(c()) * v
  }

  def grad = forAll(nodes.genVar()) {
    c => c.grad() shouldBe rule.one(c())
  }

}

