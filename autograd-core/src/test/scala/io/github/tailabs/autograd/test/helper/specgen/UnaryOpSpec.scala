package io.github.tailabs.autograd.test.helper.specgen

import io.github.tailabs.autograd.graph.Node
import io.github.tailabs.autograd.test.helper.matcher.ValueMatcherProp._
import io.github.tailabs.autograd.test.helper.gen._
import io.github.tailabs.autograd.rule.ValueRule
import io.github.tailabs.autograd.test.helper.rule.CompareRule
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

import scala.language.higherKinds


class UnaryOpSpec[U[_], T](
  d: UnaryOpExpectedBehaviorDef[U, T],
  nodes: GenNode[U, T],
  values: GenValue[U, T]
)(implicit rule: ValueRule[U, T], comparator: CompareRule[U, T]) {

  type Restriction = T => Boolean

  val default: Restriction = (_: T) => true

  private[this] def node(r: Restriction) = nodes.genNode(r)
  private[this]def sc(r: Restriction) = nodes.genScalarConstWithSource(r)
  private[this]def cc(r: Restriction) = nodes.genContainerConstWithSource(r)
  private[this]def `var`(r: Restriction) = nodes.genVarWithSource(r)

  private[this]def value(r: Restriction) = values.genValue(r)
  private[this]def cv(r: Restriction) = values.genContainerValueWithSource(r)
  private[this]def ncv(r: Restriction) = values.genNonContainerValueWithSource(r)


  def applyScalar(r: Restriction = default): Prop = {
    forAll(sc(r)) { case a: ScalarConstSample[U, T] =>
      d.op(a.node).apply() shouldBe d.applyScalar(a.src)
    }
  }

  def applyContainer(r: Restriction = default): Prop = {
    forAll(cc(r)) { case a: ContainerConstSample[U, T] =>
      d.op(a.node).apply() shouldBe d.applyContainer(a.src)
    }
  }

  def applyVar(r: Restriction = default): Prop = {
    forAll(`var`(r)) { case a: VarSample[U, T] =>
      d.op(a.node).apply() shouldBe d.applyVar(a.src)
    }
  }

  def derivVarWrtSelf(r: Restriction = default): Prop = {
    forAll(`var`(r)) { case a: VarSample[U, T]=>
      d.op(a.node).deriv(a.node) shouldBe d.derivVarWrtSelf(a.src)
    }
  }

  def derivScalarWrtUnknownVar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(sc(r1), `var`(r2)) { case (a: ScalarConstSample[U, T], b: VarSample[U, T]) =>
      d.op(a.node).deriv(b.node) shouldBe d.derivScalarWrtUnknown(a.src, b.src)
    }
  }

  def derivContainerWrtUnknownVar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(cc(r1), `var`(r2)) { case (a: ContainerConstSample[U, T], b: VarSample[U, T]) =>
      d.op(a.node).deriv(b.node) shouldBe d.derivContainerWrtUnknown(a.src, b.src)
    }
  }

  def derivVarWrtUnknownVar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(`var`(r1), `var`(r2)) { case (a: VarSample[U, T], b: VarSample[U, T]) =>
      d.op(a.node).deriv(b.node) shouldBe d.derivVarWrtUnknown(a.src, b.src)
    }
  }

  def propagateScalarWithNCValue(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(sc(r1), ncv(r2)) { case (a: ScalarConstSample[U, T], b: NonContainerValueSample[U, T]) =>
      d.op(a.node).propagate(b.value) shouldBe d.propagateScalarWithNCValue(a.src, b.src)
    }
  }

  def propagateContainerWithNCValue(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(cc(r1), ncv(r2)) { case (a: ContainerConstSample[U, T], b: NonContainerValueSample[U, T]) =>
      d.op(a.node).propagate(b.value) shouldBe d.propagateContainerWithNCValue(a.src, b.src)
    }
  }

  def propagateVarWithNCValue(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(`var`(r1), ncv(r2)) { case (a: VarSample[U, T], b: NonContainerValueSample[U, T]) =>
      d.op(a.node).propagate(b.value) shouldBe d.propagateVarWithNCValue(a.src, b.src)
    }
  }


  def propagateScalarWithCValue(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(sc(r1), cv(r2)) { case (a: ScalarConstSample[U, T], b: ContainerValueSample[U, T]) =>
      d.op(a.node).propagate(b.value) shouldBe d.propagateScalarWithCValue(a.src, b.src)
    }
  }

  def propagateContainerWithCValue(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(cc(r1), cv(r2)) { case (a: ContainerConstSample[U, T], b: ContainerValueSample[U, T]) =>
      d.op(a.node).propagate(b.value) shouldBe d.propagateContainerWithCValue(a.src, b.src)
    }
  }

  def propagateVarWithCValue(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(`var`(r1), cv(r2)) { case (a: VarSample[U, T], b: ContainerValueSample[U, T]) =>
      d.op(a.node).propagate(b.value) shouldBe d.propagateVarWithCValue(a.src, b.src)
    }
  }


  def gradScalar(r: Restriction = default): Prop = {
    forAll(sc(r)) { case a: ScalarConstSample[U, T] =>
      d.op(a.node).grad() shouldBe d.gradScalar(a.src)
    }
  }

  def gradContainer(r: Restriction = default): Prop = {
    forAll(cc(r)) { case a: ContainerConstSample[U, T]=>
      d.op(a.node).grad() shouldBe d.gradContainer(a.src)
    }
  }

  def gradVar(r: Restriction = default): Prop = {
    forAll(`var`(r)) { case a: VarSample[U, T] =>
      d.op(a.node).grad() shouldBe d.gradVar(a.src)
    }
  }

}
