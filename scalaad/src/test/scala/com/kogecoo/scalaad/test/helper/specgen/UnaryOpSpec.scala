package com.kogecoo.scalaad.test.helper.specgen

import com.kogecoo.scalaad.graph.Node
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherProp
import ValueMatcherProp._
import com.kogecoo.scalaad.test.helper.gen.{GenNode, GenValue}
import com.kogecoo.scalaad.rule.{Value, ValueRule}
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

import scala.language.higherKinds


class UnaryOpSpec[U[_], T](d: UnaryOpSpecDef[U, T], nodes: GenNode[U, T], values: GenValue[U, T])(implicit rule: ValueRule[U, T]) {

  def n = nodes.genNode
  def v = values.genValue

  def apply: Prop = forAll(n) { a: Node[U, T] =>
    d.op(a).apply() shouldBe d.applyExpectation(a)
  }

  def deriv: Prop = forAll(n, n) { (a: Node[U, T], b: Node[U, T]) =>
    d.op(a).deriv(b) shouldBe d.derivExpectation(a, b)
  }

  def derivSelf: Prop = forAll(n) { a: Node[U, T] =>
    d.op(a).deriv(a) shouldBe d.derivSelfExpectation(a)
  }

  def propagate: Prop = forAll(n, v) { (a: Node[U, T], c: Value[U, T]) =>
    d.op(a).propagate(c) shouldBe d.propagateExpectation(a, c)
  }

  def grad: Prop = forAll(n) { a: Node[U, T] =>
    d.op(a).grad() shouldBe d.gradExpectation(a)
  }

}
