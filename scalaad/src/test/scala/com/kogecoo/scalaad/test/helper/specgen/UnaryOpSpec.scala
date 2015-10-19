package com.kogecoo.scalaad.test.helper.specgen

import com.kogecoo.scalaad.graph.Node
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherProp._
import com.kogecoo.scalaad.test.helper.gen.{GenNode, GenValue}
import com.kogecoo.scalaad.rule.{Value, ValueRule}
import com.kogecoo.scalaad.test.helper.rule.CompareRule
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

import scala.language.higherKinds


class UnaryOpSpec[U[_], T](d: UnaryOpSpecDef[U, T], nodes: GenNode[U, T], values: GenValue[U, T])(implicit rule: ValueRule[U, T], comparator: CompareRule[U, T]) {

  type Restriction = T => Boolean

  val default = (_: T) => true

  def n(r: Restriction) = nodes.genNode(r)
  def v(r: Restriction) = values.genValue(r)

  def apply(restriction: Restriction = default): Prop = {
    forAll(n(restriction)) { a: Node[U, T] =>
      d.op(a).apply() shouldBe d.applyExpectation(a)
    }
  }

  def deriv(restrictionLeft: Restriction = default, restrictionRight: Restriction = default): Prop = {
    forAll(n(restrictionLeft), n(restrictionRight)) { (a: Node[U, T], b: Node[U, T]) =>
      d.op(a).deriv(b) shouldBe d.derivExpectation(a, b)
    }
  }

  def derivSelf(restriction: Restriction = default): Prop = {
    forAll(n(restriction)) { a: Node[U, T] =>
      d.op(a).deriv(a) shouldBe d.derivSelfExpectation(a)
    }
  }

  def propagate(restrictionNode: Restriction = default, restrictionValue: Restriction = default): Prop = {
    forAll(n(restrictionNode), v(restrictionValue)) { (a: Node[U, T], b: Value[U, T]) =>
      d.op(a).propagate(b) shouldBe d.propagateExpectation(a, b)
    }
  }

  def grad(restriction: Restriction = default): Prop = {
    forAll(n(restriction)) { a: Node[U, T] =>
      d.op(a).grad() shouldBe d.gradExpectation(a)
    }
  }

}
