package com.kogecoo.scalaad.breeze.test

import com.kogecoo.scalaad.breeze.test.helper.gen._
import com.kogecoo.scalaad.graph.Node
import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherProp._
import com.kogecoo.scalaad.test.helper.rule.CompareRule
import com.kogecoo.scalaad.test.helper.specgen.{BinaryOpExpectedBehaviorDef, BinaryOpSpecBase}
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

import scala.language.higherKinds


class MatrixDotBinaryOpSpec[U[_], T](
  d: BinaryOpExpectedBehaviorDef[U, T],
  shape: MMulMatrixShapeGen,
  nodes: ShapeRestrictedNodeGen[U, T],
  values: ShapeRestrictedValueGen[U, T]
)(implicit rule: ValueRule[U, T], shouldBe: CompareRule[U, T]) extends BinaryOpSpecBase[U, T] {

  val pairs = new MMulShapeRestrictedNodePairGen[U, T](shape, nodes)
  def node = nodes.genNode _
  def ncValue = values.genNonContainerValueWithSource _
  def cValue = values.genContainerValueWithSource _
  def sc = nodes.genScalarConstWithSource _
  def cc = nodes.genContainerConstWithSource _
  def genVar = nodes.genVarWithSource _

  def mmulableShape(shapes: (MatrixShape, MatrixShape)): MatrixShape = {
    MatrixShape(shapes._1.rows, shapes._2.cols)
  }

  override def applyScalarScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genSS(r1, r2)) { case (a: ScalarConstSample[U, T], b: ScalarConstSample[U, T]) =>
      d.op(a.node, b.node).apply() shouldBe d.applyScalarScalar(a.src, b.src)
    }
  }

  override def applyScalarContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genSC(r1, r2)) { case (a: ScalarConstSample[U, T], b: ContainerConstSample[U, T])=>
      d.op(a.node, b.node).apply() shouldBe d.applyScalarContainer(a.src, b.src)
    }
  }

  override def applyScalarVar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genSV(r1, r2)) { case (a: ScalarConstSample[U, T], b: VarSample[U, T])=>
      d.op(a.node, b.node).apply() shouldBe d.applyScalarVar(a.src, b.src)
    }
  }

  override def applyContainerScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genCS(r1, r2)) { case (a: ContainerConstSample[U, T], b: ScalarConstSample[U, T]) =>
      d.op(a.node, b.node).apply() shouldBe d.applyContainerScalar(a.src, b.src)
    }
  }

  override def applyContainerContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genCC(r1, r2)) { case (a: ContainerConstSample[U, T], b: ContainerConstSample[U, T])=>
      d.op(a.node, b.node).apply() shouldBe d.applyContainerContainer(a.src, b.src)
    }
  }

  override def applyContainerVar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genCV(r1, r2)) { case (a: ContainerConstSample[U, T], b: VarSample[U, T])=>
      d.op(a.node, b.node).apply() shouldBe d.applyContainerVar(a.src, b.src)
    }
  }

  override def applyVarScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genVS(r1, r2)) { case (a: VarSample[U, T], b: ScalarConstSample[U, T]) =>
      d.op(a.node, b.node).apply() shouldBe d.applyVarScalar(a.src, b.src)
    }
  }

  override def applyVarContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genVC(r1, r2)) { case (a: VarSample[U, T], b: ContainerConstSample[U, T])=>
      d.op(a.node, b.node).apply() shouldBe d.applyVarContainer(a.src, b.src)
    }
  }

  override def applyVarVar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genVV(r1, r2)) { case (a: VarSample[U, T], b: VarSample[U, T])=>
      d.op(a.node, b.node).apply() shouldBe d.applyVarVar(a.src, b.src)
    }
  }

  override def derivScalarVarWrtRight(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genSV(r1, r2)) { case (a: ScalarConstSample[U, T], b: VarSample[U, T]) =>
      d.op(a.node, b.node).deriv(b.node) shouldBe d.derivScalarVarWrtRight(a.src, b.src)
    }
  }

  override def derivScalarVarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, sc, v1) <- pairs.genSVWithShape(r1, r2)
      v2               <- genVar(mmulableShape(shapes), r3)
    } yield (sc, v1, v2)

    forAll(g) { case (a: ScalarConstSample[U, T], b: VarSample[U, T], c: VarSample[U, T]) =>
      d.op(a.node, b.node).deriv(c.node) shouldBe d.derivScalarVarWrtUnknown(a.src, b.src, c.src)
    }
  }

  override def derivContainerVarWrtRight(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genCV(r1, r2)) { case (a: ContainerConstSample[U, T], b: VarSample[U, T])=>
      d.op(a.node, b.node).deriv(b.node) shouldBe d.derivContainerVarWrtRight(a.src, b.src)
    }
  }

  override def derivContainerVarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, cc, v1) <- pairs.genCVWithShape(r1, r2)
      v2               <- genVar(mmulableShape(shapes), r3)
    } yield (cc, v1, v2)

    forAll(g) { case (a: ContainerConstSample[U, T], b: VarSample[U, T], c: VarSample[U, T]) =>
      d.op(a.node, b.node).deriv(c.node) shouldBe d.derivContainerVarWrtUnknown(a.src, b.src, c.src)
    }
  }

  override def derivVarScalarWrtLeft(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genVS(r1, r2)) { case (a: VarSample[U, T], b: ScalarConstSample[U, T]) =>
      d.op(a.node, b.node).deriv(a.node) shouldBe d.derivVarScalarWrtLeft(a.src, b.src)
    }
  }

  override def derivVarScalarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
     val g = for {
      (shapes, v1, sc) <- pairs.genVSWithShape(r1, r2)
      v2               <- genVar(mmulableShape(shapes), r3)
    } yield (v1, sc, v2)

    forAll(g) { case (a: VarSample[U, T], b: ScalarConstSample[U, T], c: VarSample[U, T]) =>
      d.op(a.node, b.node).deriv(c.node) shouldBe d.derivVarScalarWrtUnknown(a.src, b.src, c.src)
    }
  }

  override def derivVarContainerWrtLeft(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genVC(r1, r2)) { case (a: VarSample[U, T], b: ContainerConstSample[U, T]) =>
      d.op(a.node, b.node).deriv(a.node) shouldBe d.derivVarContainerWrtLeft(a.src, b.src)
    }
  }

  override def derivVarContainerWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, v1, cc) <- pairs.genVCWithShape(r1, r2)
      v2               <- genVar(mmulableShape(shapes), r3)
    } yield (v1, cc, v2)

    forAll(g) { case (a: VarSample[U, T], b: ContainerConstSample[U, T], c: Node[U, T]) =>
      d.op(a.node, b.node).deriv(c.node) shouldBe d.derivVarContainerWrtUnknown(a.src, b.src, c.src)
    }
  }

  override def derivVarVarWrtLeft(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genVV(r1, r2)) { case (a: VarSample[U, T], b: VarSample[U, T])=>
      d.op(a.node, b.node).deriv(a.node) shouldBe d.derivVarVarWrtLeft(a.src, b.src)
    }
  }

  override def derivVarVarWrtRight(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genVV(r1, r2)) { case (a: VarSample[U, T], b: VarSample[U, T])=>
      d.op(a.node, b.node).deriv(b.node) shouldBe d.derivVarVarWrtRight(a.src, b.src)
    }
  }

  override def derivVarVarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, v1, v2) <- pairs.genVVWithShape(r1, r2)
      v3               <- genVar(mmulableShape(shapes), r3)
    } yield (v1, v2, v3)

    forAll(g) { case (a: VarSample[U, T], b: VarSample[U, T], c: VarSample[U, T]) =>
      d.op(a.node, b.node).deriv(c.node) shouldBe d.derivVarVarWrtUnknown(a.src, b.src, c.src)
    }
  }

  override def derivVarVarWrtSelf(r: Restriction = default): Prop = {
    val vGen = for {
      s <- shape.genShape()
      v <- nodes.genVarWithSource(mmulableShape(s), r)
    } yield v

    forAll(vGen) { case a: VarSample[U, T] =>
      d.op(a.node, a.node.T).deriv(a.node) shouldBe d.derivVarVarWrtSelf(a.src)
    }
  }


  override def propagateScalarScalarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    forAll(pairs.genSS(r1, r2), ncValue(r3)) { case ((a: ScalarConstSample[U, T], b: ScalarConstSample[U, T]), c: NonContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarScalarWithNCValue(a.src, b.src, c.src)
    }
  }

  override def propagateScalarContainerWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    forAll(pairs.genSC(r1, r2), ncValue(r3)) { case ((a: ScalarConstSample[U, T], b: ContainerConstSample[U, T]), c: NonContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarContainerWithNCValue(a.src, b.src, c.src)
    }
  }

  override def propagateScalarVarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    forAll(pairs.genSV(r1, r2), ncValue(r3)) { case ((a: ScalarConstSample[U, T], b: VarSample[U, T]), c: NonContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarVarWithNCValue(a.src, b.src, c.src)
    }
  }

  override def propagateContainerScalarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    forAll(pairs.genCS(r1, r2), ncValue(r3)) { case ((a: ContainerConstSample[U, T], b: ScalarConstSample[U, T]), c: NonContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerScalarWithNCValue(a.src, b.src, c.src)
    }
  }

  override def propagateContainerContainerWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    forAll(pairs.genCC(r1, r2), ncValue(r3)) { case ((a: ContainerConstSample[U, T], b: ContainerConstSample[U, T]), c: NonContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerContainerWithNCValue(a.src, b.src, c.src)
    }
  }

  override def propagateContainerVarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    forAll(pairs.genCV(r1, r2), ncValue(r3)) { case ((a: ContainerConstSample[U, T], b: VarSample[U, T]), c: NonContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerVarWithNCValue(a.src, b.src, c.src)
    }
  }


  override def propagateVarScalarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    forAll(pairs.genVS(r1, r2), ncValue(r3)) { case ((a: VarSample[U, T], b: ScalarConstSample[U, T]), c: NonContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarScalarWithNCValue(a.src, b.src, c.src)
    }
  }

  override def propagateVarContainerWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    forAll(pairs.genVC(r1, r2), ncValue(r3)) { case ((a: VarSample[U, T], b: ContainerConstSample[U, T]), c: NonContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarContainerWithNCValue(a.src, b.src, c.src)
    }
  }

  override def propagateVarVarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    forAll(pairs.genVV(r1, r2), ncValue(r3)) { case ((a: VarSample[U, T], b: VarSample[U, T]), c: NonContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarVarWithNCValue(a.src, b.src, c.src)
    }
  }


  override def propagateScalarScalarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, sc1, sc2) <- pairs.genSSWithShape(r1, r2)
      value              <- cValue(mmulableShape(shapes), r3)
    } yield (sc1, sc2, value)

    forAll(g) { case (a: ScalarConstSample[U, T], b: ScalarConstSample[U, T], c: ContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarScalarWithCValue(a.src, b.src, c.src)
    }
  }

  override def propagateScalarContainerWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
     val g = for {
      (shapes, sc, cc) <- pairs.genSCWithShape(r1, r2)
      value              <- cValue(mmulableShape(shapes), r3)
    } yield (sc, cc, value)

    forAll(g) { case (a: ScalarConstSample[U, T], b: ContainerConstSample[U, T], c: ContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarContainerWithCValue(a.src, b.src, c.src)
    }
  }

  override def propagateScalarVarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, sc, v) <- pairs.genSVWithShape(r1, r2)
      value               <- cValue(mmulableShape(shapes), r3)
    } yield (sc, v, value)

    forAll(g) { case (a: ScalarConstSample[U, T], b: VarSample[U, T], c: ContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarVarWithCValue(a.src, b.src, c.src)
    }
  }

  override def propagateContainerScalarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, cc, sc) <- pairs.genCSWithShape(r1, r2)
      value            <- cValue(mmulableShape(shapes), r3)
    } yield (cc, sc, value)

    forAll(g) { case (a: ContainerConstSample[U, T], b: ScalarConstSample[U, T], c: ContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerScalarWithCValue(a.src, b.src, c.src)
    }
  }

  override def propagateContainerContainerWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, cc1, cc2) <- pairs.genCCWithShape(r1, r2)
      value              <- cValue(mmulableShape(shapes), r3)
    } yield (cc1, cc2, value)

    forAll(g) { case (a: ContainerConstSample[U, T], b: ContainerConstSample[U, T], c: ContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerContainerWithCValue(a.src, b.src, c.src)
    }
  }

  override def propagateContainerVarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, cc, v) <- pairs.genCVWithShape(r1, r2)
      value           <- cValue(mmulableShape(shapes), r3)
    } yield (cc, v, value)

    forAll(g) { case (a: ContainerConstSample[U, T], b: VarSample[U, T], c: ContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerVarWithCValue(a.src, b.src, c.src)
    }
  }


  override def propagateVarScalarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, v, sc) <- pairs.genVSWithShape(r1, r2)
      value           <- cValue(mmulableShape(shapes), r3)
    } yield (v, sc, value)

    forAll(g) { case (a: VarSample[U, T], b: ScalarConstSample[U, T], c: ContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarScalarWithCValue(a.src, b.src, c.src)
    }
  }

  override def propagateVarContainerWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, v, cc) <- pairs.genVCWithShape(r1, r2)
      value           <- cValue(mmulableShape(shapes), r3)
    } yield (v, cc, value)

    forAll(g) { case (a: VarSample[U, T], b: ContainerConstSample[U, T], c: ContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarContainerWithCValue(a.src, b.src, c.src)
    }
  }

  override def propagateVarVarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
    val g = for {
      (shapes, v1, v2) <- pairs.genVVWithShape(r1, r2)
      value              <- cValue(mmulableShape(shapes), r3)
    } yield (v1, v2, value)

    forAll(g) { case (a: VarSample[U, T], b: VarSample[U, T], c: ContainerValueSample[U, T]) =>
      d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarVarWithCValue(a.src, b.src, c.src)
    }
  }


  override def gradScalarScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genSS(r1, r2)) { case (a: ScalarConstSample[U, T], b: ScalarConstSample[U, T]) =>
      d.op(a.node, b.node).grad() shouldBe d.gradScalarScalar(a.src, b.src)
    }
  }

  override def gradScalarContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genSC(r1, r2)) { case (a: ScalarConstSample[U, T], b: ContainerConstSample[U, T]) =>
      d.op(a.node, b.node).grad() shouldBe d.gradScalarContainer(a.src, b.src)
    }
  }

  override def gradScalarVar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genSV(r1, r2)) { case (a: ScalarConstSample[U, T], b: VarSample[U, T]) =>
      d.op(a.node, b.node).grad() shouldBe d.gradScalarVar(a.src, b.src)
    }
  }

  override def gradContainerScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genCS(r1, r2)) { case (a: ContainerConstSample[U, T], b: ScalarConstSample[U, T]) =>
      d.op(a.node, b.node).grad() shouldBe d.gradContainerScalar(a.src, b.src)
    }
  }

  override def gradContainerContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genCC(r1, r2)) { case (a: ContainerConstSample[U, T], b: ContainerConstSample[U, T]) =>
      d.op(a.node, b.node).grad() shouldBe d.gradContainerContainer(a.src, b.src)
    }
  }

  override def gradContainerVar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genCV(r1, r2)) { case (a: ContainerConstSample[U, T], b: VarSample[U, T]) =>
      d.op(a.node, b.node).grad() shouldBe d.gradContainerVar(a.src, b.src)
    }
  }

  override def gradVarScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genVS(r1, r2)) { case (a: VarSample[U, T], b: ScalarConstSample[U, T]) =>
      d.op(a.node, b.node).grad() shouldBe d.gradVarScalar(a.src, b.src)
    }
  }

  override def gradVarContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genVC(r1, r2)) { case (a: VarSample[U, T], b: ContainerConstSample[U, T]) =>
      d.op(a.node, b.node).grad() shouldBe d.gradVarContainer(a.src, b.src)
    }
  }

  override def gradVarVar(r1: Restriction = default, r2: Restriction = default): Prop = {
    forAll(pairs.genVV(r1, r2)) { case (a: VarSample[U, T], b: VarSample[U, T]) =>
      d.op(a.node, b.node).grad() shouldBe d.gradVarVar(a.src, b.src)
    }
  }

}
