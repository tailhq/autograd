package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph.{N0, N1, N2, Var0}
import com.kogecoo.scalaad.test.{NodeSpecBase, SpecBackend}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


trait BinaryOp00SpecBase extends NodeSpecBase { self: Properties with SpecBackend =>

  import com.kogecoo.scalaad.test.SpecBackendHelper.Implicits._

  def op(a: N0, b: N0): N0

  def op(a: String, b: String): String


  def expectApplyOp(a: N0, b: N0): T0

  def leftDeriv(a: T0, b: T0): T0

  def rightDeriv(a: T0, b: T0): T0

  def leftRightDeriv(a: T0): T0 = add(leftDeriv(a, a), rightDeriv(a, a))

  def expectReverseLeft0(a: Var0, b: N0, c: N0): T0 = mul(leftDeriv(a.toT0, b.toT0), c.toT0)

  def expectReverseLeft1(a: Var0, b: N0, c: N1): T1 = broadcast1(c.toT1, mul(_, leftDeriv(a.toT0, b.toT0)))

  def expectReverseLeft2(a: Var0, b: N0, c: N2): T2 = broadcast2(c.toT2, mul(_, leftDeriv(a.toT0, b.toT0)))

  def expectReverseRight0(a: N0, b: Var0, c: N0): T0 = mul(rightDeriv(a.toT0, b.toT0), c.toT0)

  def expectReverseRight1(a: N0, b: Var0, c: N1): T1 = broadcast1(c.toT1, mul(_, rightDeriv(a.toT0, b.toT0)))

  def expectReverseRight2(a: N0, b: Var0, c: N2): T2 = broadcast2(c.toT2, mul(_, rightDeriv(a.toT0, b.toT0)))

  def expectReverseLeftRight0(a: Var0, b: N0): T0 = mul(leftRightDeriv(a.toT0), b.toT0)

  def expectReverseLeftRight1(a: Var0, b: N1): T1 = broadcast1(b.toT1, mul(_, leftRightDeriv(a.toT0)))

  def expectReverseLeftRight2(a: Var0, b: N2): T2 = broadcast2(b.toT2, mul(_, leftRightDeriv(a.toT0)))

  def genLeftArgN0ForSpecBase: Gen[N0] = genN0()

  def genLeftArgNV0ForSpecBase: Gen[N0] = genNV0()

  def genLeftArgV0ForSpecBase: Gen[Var0] = genV0()

  def genRightArgN0ForSpecBase: Gen[N0] = genN0()

  def genRightArgNV0ForSpecBase: Gen[N0] = genNV0()

  def genRightArgV0ForSpecBase: Gen[Var0] = genV0()

  def genLeftRightArgV0ForSpecBase: Gen[Var0] = genV0()

  def genAdjointN0ForSpecBase: Gen[N0] = genN0()

  def genAdjointN1ForSpecBase: Gen[N1] = genN1()

  def genAdjointN2ForSpecBase: Gen[N2] = genN2()


  property("eval") = forAll(genLeftArgN0ForSpecBase, genRightArgN0ForSpecBase) { (a: N0, b: N0) =>
    op(a, b) shouldCloseTo expectApplyOp(a, b)
  }

  property(s"${op("node0", "node0")} forward w.r.t node0") =
    forAll(genLeftArgN0ForSpecBase, genRightArgN0ForSpecBase, genAdjointN0ForSpecBase) { (a: N0, b: N0, c: N0) =>
      op(a, b).forward[N0, N0](c) shouldCloseTo zero0
  }

  property(s"${op("node0", "node0")} forward w.r.t node1") =
    forAll(genLeftArgN0ForSpecBase, genRightArgN0ForSpecBase, genAdjointN1ForSpecBase){ (a: N0, b: N0, c: N1) =>
      op(a, b).forward[N1, N1](c) shouldCloseTo zero1(c)
  }

  property(s"${op("node0", "node0")} forward w.r.t node2") =
    forAll(genLeftArgN0ForSpecBase, genRightArgN0ForSpecBase, genAdjointN2ForSpecBase) { (a: N0, b: N0, c: N2) =>
      op(a, b).forward[N2, N2](c) shouldCloseTo zero2(c)
  }

  property(s"${op("var0", "node0")} forward w.r.t left") =
    forAll(genLeftArgV0ForSpecBase, genRightArgN0ForSpecBase) { (a: Var0, b: N0) =>
      op(a, b).forward[N0, N0](a) shouldCloseTo leftDeriv(a.toT0, b.toT0)
  }

  property(s"${op("node0", "var0")} forward w.r.t right") =
    forAll(genLeftArgN0ForSpecBase, genRightArgV0ForSpecBase) { (a: N0, b: Var0) =>
      op(a, b).forward[N0, N0](b) shouldCloseTo rightDeriv(a.toT0, b.toT0)
  }

  property(s"${op("var0", "var0")} forward w.r.t self") =
    forAll(genLeftRightArgV0ForSpecBase) { (a: Var0) =>
      op(a, a).forward[N0, N0](a) shouldCloseTo leftRightDeriv(a.toT0)
  }

  property(s"${op("nonvar0", "nonvar0")} reverse node0") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN0ForSpecBase) { (a: N0, b: N0, c: N0) =>
      op(a, b).reverse(c).size == 0
  }

  property(s"${op("nonvar0", "nonvar0")} reverse node1") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN1ForSpecBase) { (a: N0, b: N0, c: N1) =>
      op(a, b).reverse(c).size == 0
  }

  property(s"${op("nonvar0", "nonvar0")} reverse node2") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN2ForSpecBase) { (a: N0, b: N0, c: N2) =>
      op(a, b).reverse(c).size == 0
  }

  property(s"${op("var0", "nonvar0")} reverse node0") =
    forAll(genLeftArgV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN0ForSpecBase) { (a: Var0, b: N0, c: N0) =>
      val g = op(a, b).reverse(c)
      g(a).get.asInstanceOf[N0] shouldCloseTo expectReverseLeft0(a, b, c)
  }

  property(s"${op("var0", "nonvar0")} reverse node1") =
    forAll(genLeftArgV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN1ForSpecBase) { (a: Var0, b: N0, c: N1) =>
      val g = op(a, b).reverse(c)
      g(a).get.asInstanceOf[N1] shouldCloseTo expectReverseLeft1(a, b, c)
  }

  property(s"${op("var0", "nonvar0")} reverse node2") =
    forAll(genLeftArgV0ForSpecBase, genRightArgNV0ForSpecBase, genAdjointN2ForSpecBase) { (a: Var0, b: N0, c: N2) =>
      val g = op(a, b).reverse(c)
      g(a).get.asInstanceOf[N2] shouldCloseTo expectReverseLeft2(a, b, c)
  }

  property(s"${op("nonvar0", "var0")} reverse node0") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgV0ForSpecBase, genAdjointN0ForSpecBase) { (a: N0, b: Var0, c: N0) =>
      val g = op(a, b).reverse(c)
      g(b).get.asInstanceOf[N0] shouldCloseTo expectReverseRight0(a, b, c)
  }

  property(s"${op("nonvar0", "var0")} reverse node1") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgV0ForSpecBase, genAdjointN1ForSpecBase) { (a: N0, b: Var0, c: N1) =>
      val g = op(a, b).reverse(c)
      g(b).get.asInstanceOf[N1] shouldCloseTo expectReverseRight1(a, b, c)
  }

  property(s"${op("nonvar0", "var0")} reverse node2") =
    forAll(genLeftArgNV0ForSpecBase, genRightArgV0ForSpecBase, genAdjointN2ForSpecBase) { (a: N0, b: Var0, c: N2) =>
      val g = op(a, b).reverse(c)
      g(b).get.asInstanceOf[N2] shouldCloseTo expectReverseRight2(a, b, c)
  }

  property(s"${op("var0", "var0")} reverse node0") =
    forAll(genLeftRightArgV0ForSpecBase, genAdjointN0ForSpecBase) { (a: Var0, b: N0) =>
      val g = op(a, a).reverse(b)
      g(a).get.asInstanceOf[N0] shouldCloseTo expectReverseLeftRight0(a, b)
  }

  property(s"${op("var0", "var0")} reverse node1") =
    forAll(genLeftRightArgV0ForSpecBase, genAdjointN1ForSpecBase) { (a: Var0, b: N1) =>
      val g = op(a, a).reverse(b)
      g(a).get.asInstanceOf[N1] shouldCloseTo expectReverseLeftRight1(a, b)
  }

  property(s"${op("var0", "var0")} reverse node2") =
    forAll(genLeftRightArgV0ForSpecBase, genAdjointN2ForSpecBase) { (a: Var0, b: N2) =>
      val g = op(a, a).reverse(b)
      g(a).get.asInstanceOf[N2] shouldCloseTo expectReverseLeftRight2(a, b)
  }

}




