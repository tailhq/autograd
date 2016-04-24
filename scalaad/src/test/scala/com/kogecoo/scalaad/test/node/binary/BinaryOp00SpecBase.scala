package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph.{V0, V1, V2, Var0}
import com.kogecoo.scalaad.test.{NodeSpecBase, SpecBackend}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


trait BinaryOp00SpecBase extends NodeSpecBase { self: Properties with SpecBackend =>

  import com.kogecoo.scalaad.test.SpecBackendHelper.Implicits._

  def op(a: V0, b: V0): V0

  def op(a: String, b: String): String


  def expectApplyOp(a: V0, b: V0): T0

  def leftDeriv(a: T0, b: T0): T0

  def rightDeriv(a: T0, b: T0): T0

  def leftRightDeriv(a: T0): T0 = add(leftDeriv(a, a), rightDeriv(a, a))

  def expectReverseLeft0(a: Var0, b: V0, c: V0): T0 = mul(leftDeriv(a.toT0, b.toT0), c.toT0)

  def expectReverseLeft1(a: Var0, b: V0, c: V1): T1 = broadcast1(c.toT1, mul(_, leftDeriv(a.toT0, b.toT0)))

  def expectReverseLeft2(a: Var0, b: V0, c: V2): T2 = broadcast2(c.toT2, mul(_, leftDeriv(a.toT0, b.toT0)))

  def expectReverseRight0(a: V0, b: Var0, c: V0): T0 = mul(rightDeriv(a.toT0, b.toT0), c.toT0)

  def expectReverseRight1(a: V0, b: Var0, c: V1): T1 = broadcast1(c.toT1, mul(_, rightDeriv(a.toT0, b.toT0)))

  def expectReverseRight2(a: V0, b: Var0, c: V2): T2 = broadcast2(c.toT2, mul(_, rightDeriv(a.toT0, b.toT0)))

  def expectReverseLeftRight0(a: Var0, b: V0): T0 = mul(leftRightDeriv(a.toT0), b.toT0)

  def expectReverseLeftRight1(a: Var0, b: V1): T1 = broadcast1(b.toT1, mul(_, leftRightDeriv(a.toT0)))

  def expectReverseLeftRight2(a: Var0, b: V2): T2 = broadcast2(b.toT2, mul(_, leftRightDeriv(a.toT0)))

  def genLeftArgN0ForSpecBase: Gen[V0] = genN0()

  def genLeftArgNV0ForSpecBase: Gen[V0] = genNV0()

  def genLeftArgV0ForSpecBase: Gen[Var0] = genV0()

  def genRightArgN0ForSpecBase: Gen[V0] = genN0()

  def genRightArgNV0ForSpecBase: Gen[V0] = genNV0()

  def genRightArgV0ForSpecBase: Gen[Var0] = genV0()

  def genLeftRightArgV0ForSpecBase: Gen[Var0] = genV0()

  def genAdjointN0ForSpecBase: Gen[V0] = genN0()

  def genAdjointN1ForSpecBase: Gen[V1] = genN1()

  def genAdjointN2ForSpecBase: Gen[V2] = genN2()


  property("eval") = forAll(genLeftArgN0ForSpecBase, genRightArgN0ForSpecBase) { (a: V0, b: V0) =>
    op(a, b) shouldCloseTo expectApplyOp(a, b)
  }

  property(s"${op("node0", "node0")} forward w.r.t node0") =
    forAll(genLeftArgN0ForSpecBase, genRightArgN0ForSpecBase, genAdjointN0ForSpecBase) { (a: V0, b: V0, c: V0) =>
      op(a, b).forward[V0, V0](c) shouldCloseTo zero0
  }

  property(s"${op("node0", "node0")} forward w.r.t node1") =
    forAll(genLeftArgN0ForSpecBase, genRightArgN0ForSpecBase, genAdjointN1ForSpecBase){ (a: V0, b: V0, c: V1) =>
      op(a, b).forward[V1, V1](c) shouldCloseTo zero1(c)
  }

  property(s"${op("node0", "node0")} forward w.r.t node2") =
    forAll(genLeftArgN0ForSpecBase, genRightArgN0ForSpecBase, genAdjointN2ForSpecBase) { (a: V0, b: V0, c: V2) =>
      op(a, b).forward[V2, V2](c) shouldCloseTo zero2(c)
  }

  property(s"${op("var0", "node0")} forward w.r.t left") =
    forAll(genLeftArgV0ForSpecBase, genRightArgN0ForSpecBase) { (a: Var0, b: V0) =>
      op(a, b).forward[V0, V0](a) shouldCloseTo leftDeriv(a.toT0, b.toT0)
  }

  property(s"${op("node0", "var0")} forward w.r.t right") =
    forAll(genLeftArgN0ForSpecBase, genRightArgV0ForSpecBase) { (a: V0, b: Var0) =>
      op(a, b).forward[V0, V0](b) shouldCloseTo rightDeriv(a.toT0, b.toT0)
  }

  property(s"${op("var0", "var0")} forward w.r.t self") =
    forAll(genLeftRightArgV0ForSpecBase) { (a: Var0) =>
      op(a, a).forward[V0, V0](a) shouldCloseTo leftRightDeriv(a.toT0)
  }

  property(s"${op("nonvar0", "nonvar0")} reverse node0") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN0ForSpecBase) { (a: V0, b: V0, c: V0) =>
      op(a, b).reverse(c).size == 0
  }

  property(s"${op("nonvar0", "nonvar0")} reverse node1") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN1ForSpecBase) { (a: V0, b: V0, c: V1) =>
      op(a, b).reverse(c).size == 0
  }

  property(s"${op("nonvar0", "nonvar0")} reverse node2") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN2ForSpecBase) { (a: V0, b: V0, c: V2) =>
      op(a, b).reverse(c).size == 0
  }

  property(s"${op("var0", "nonvar0")} reverse node0") =
    forAll(genLeftArgV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN0ForSpecBase) { (a: Var0, b: V0, c: V0) =>
      val g = op(a, b).reverse(c)
      g(a).get.asInstanceOf[V0] shouldCloseTo expectReverseLeft0(a, b, c)
  }

  property(s"${op("var0", "nonvar0")} reverse node1") =
    forAll(genLeftArgV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN1ForSpecBase) { (a: Var0, b: V0, c: V1) =>
      val g = op(a, b).reverse(c)
      g(a).get.asInstanceOf[V1] shouldCloseTo expectReverseLeft1(a, b, c)
  }

  property(s"${op("var0", "nonvar0")} reverse node2") =
    forAll(genLeftArgV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN2ForSpecBase) { (a: Var0, b: V0, c: V2) =>
      val g = op(a, b).reverse(c)
      g(a).get.asInstanceOf[V2] shouldCloseTo expectReverseLeft2(a, b, c)
  }

  property(s"${op("nonvar0", "var0")} reverse node0") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgV0ForSpecBase, genAdjointN0ForSpecBase) { (a: V0, b: Var0, c: V0) =>
      val g = op(a, b).reverse(c)
      g(b).get.asInstanceOf[V0] shouldCloseTo expectReverseRight0(a, b, c)
  }

  property(s"${op("nonvar0", "var0")} reverse node1") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgV0ForSpecBase, genAdjointN1ForSpecBase) { (a: V0, b: Var0, c: V1) =>
      val g = op(a, b).reverse(c)
      g(b).get.asInstanceOf[V1] shouldCloseTo expectReverseRight1(a, b, c)
  }

  property(s"${op("nonvar0", "var0")} reverse node2") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgV0ForSpecBase, genAdjointN2ForSpecBase) { (a: V0, b: Var0, c: V2) =>
      val g = op(a, b).reverse(c)
      g(b).get.asInstanceOf[V2] shouldCloseTo expectReverseRight2(a, b, c)
  }

  property(s"${op("var0", "var0")} reverse node0") =
    forAll(genLeftRightArgV0ForSpecBase, genAdjointN0ForSpecBase) { (a: Var0, b: V0) =>
      val g = op(a, a).reverse(b)
      g(a).get.asInstanceOf[V0] shouldCloseTo expectReverseLeftRight0(a, b)
  }

  property(s"${op("var0", "var0")} reverse node1") =
    forAll(genLeftRightArgV0ForSpecBase, genAdjointN1ForSpecBase) { (a: Var0, b: V1) =>
      val g = op(a, a).reverse(b)
      g(a).get.asInstanceOf[V1] shouldCloseTo expectReverseLeftRight1(a, b)
  }

  property(s"${op("var0", "var0")} reverse node2") =
    forAll(genLeftRightArgV0ForSpecBase, genAdjointN2ForSpecBase) { (a: Var0, b: V2) =>
      val g = op(a, a).reverse(b)
      g(a).get.asInstanceOf[V2] shouldCloseTo expectReverseLeftRight2(a, b)
  }

}




