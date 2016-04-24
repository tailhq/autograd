package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph.{V0, V1, V2, Var1}
import com.kogecoo.scalaad.test.{NodeSpecBase, SpecBackend}
import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll


trait BinaryOp11SpecBase extends NodeSpecBase { self: Properties with SpecBackend =>

  import com.kogecoo.scalaad.test.SpecBackendHelper.Implicits._

  def op(a: V1, b: V1): V1

  def op(a: String, b: String): String


  def expectApplyOp(a: V1, b: V1): T1

  def leftDeriv(a: T0, b: T0): T0

  def leftDeriv(a: V1, b: V1): T1 = elementwise1(a.toT1, b.toT1, leftDeriv)

  def rightDeriv(a: T0, b: T0): T0

  def rightDeriv(a: V1, b: V1): T1 = elementwise1(a.toT1, b.toT1, rightDeriv)

  def leftRightDeriv(a: T0): T0 = add(leftDeriv(a, a), rightDeriv(a, a))

  def leftRightDeriv(a: V1): T1 = elementwise1(leftDeriv(a, a), rightDeriv(a, a), add)

  def expectReverseLeft0(a: Var1, b: V1, c: V0): T1 = broadcast1(leftDeriv(a, b), (x: T0) => mul(x, c.toT0))

  def expectReverseLeft1(a: Var1, b: V1, c: V1): T1 = elementwise1(c.toT1, leftDeriv(a, b), mul)

  def expectReverseLeft2(a: Var1, b: V1, c: V2): T2 = columnwise(c.toT2, leftDeriv(a, b), mul)

  def expectReverseRight0(a: V1, b: Var1, c: V0): T1 = broadcast1(rightDeriv(a, b), (x: T0) => mul(x, c.toT0))

  def expectReverseRight1(a: V1, b: Var1, c: V1): T1 = elementwise1(c.toT1, rightDeriv(a, b), mul)

  def expectReverseRight2(a: V1, b: Var1, c: V2): T2 = columnwise(c.toT2, rightDeriv(a, b), mul)

  def expectReverseLeftRight0(a: Var1, b: V0): T1 = broadcast1(leftRightDeriv(a), (x: T0) => mul(x, b.toT0))

  def expectReverseLeftRight1(a: Var1, b: V1): T1 = elementwise1(b.toT1, leftRightDeriv(a), mul)

  def expectReverseLeftRight2(a: Var1, b: V2): T2 = columnwise(b.toT2, leftRightDeriv(a), mul)


  def genAdjointN0ForSpecBase: Gen[V0] = genN0()

  def genArgV1ForSpecBase: Gen[Var1] = genV1()

  def genArgV1_N1_ForSpecBase: Gen[(Var1, V1)] = genV1_N1()

  def genArgV1_N2_ForSpecBase: Gen[(Var1, V2)] = genV1_RowEquivN2()

  def genArgN1_ArgN1_ForSpecBase: Gen[(V1, V1)] = genN1_N1()

  def genArgNV1_ArgNV1_ForSpecBase: Gen[(V1, V1)] = genNV1_NV1()

  def genArgV1_ArgN1_ForSpecBase: Gen[(Var1, V1)] = genV1_N1()

  def genArgV1_ArgNV1_ForSpecBase: Gen[(Var1, V1)] = genV1_NV1()

  def genArgN1_ArgV1_ForSpecBase: Gen[(V1, Var1)] = genN1_V1()

  def genArgNV1_ArgV1_ForSpecBase: Gen[(V1, Var1)] = genNV1_V1()

  def genArgN1_ArgN1_N1_ForSpecBase: Gen[(V1, V1, V1)] = genN1_N1_N1()

  def genArgNV1_ArgNV1_N1_ForSpecBase: Gen[(V1, V1, V1)] = genNV1_NV1_N1()

  def genArgNV1_ArgNV1_N2_ForSpecBase: Gen[(V1, V1, V2)] = genNV1_NV1_RowEquivN2()

  def genArgV1_ArgNV1_N1_ForSpecBase: Gen[(Var1, V1, V1)] = genV1_NV1_N1()

  def genArgV1_ArgNV1_N2_ForSpecBase: Gen[(Var1, V1, V2)] = genV1_NV1_RowEquivN2()

  def genArgNV1_ArgV1_N1_ForSpecBase: Gen[(V1, Var1, V1)] = genNV1_V1_N1()

  def genArgNV1_ArgV1_N2_ForSpecBase: Gen[(V1, Var1, V2)] = genNV1_V1_RowEquivN2()


  property("eval") = forAll(genArgN1_ArgN1_ForSpecBase) { case (a: V1, b: V1) =>
    op(a, b) shouldCloseTo expectApplyOp(a, b)
  }

  property(s"${op("node1", "node1")} forward w.r.t node0") =
    forAll(genArgN1_ArgN1_ForSpecBase, genAdjointN0ForSpecBase) { case ((a: V1, b: V1), c: V0) =>
      op(a, b).forward[V0, V1](c) shouldCloseTo zero1(a)
  }

  property(s"${op("node1", "node1")} forward w.r.t node1") =
    forAll(genArgN1_ArgN1_N1_ForSpecBase){ case (a: V1, b: V1, c: V1) =>
      op(a, b).forward[V1, V2](c) shouldCloseTo zero2(a, c)
  }

  property(s"${op("var1", "node1")} forward w.r.t left") =
    forAll(genArgV1_ArgN1_ForSpecBase) { case (a: Var1, b: V1) =>
      op(a, b).forward[V1, V2](a) shouldCloseTo diag(leftDeriv(a, b))
  }

  property(s"${op("node1", "var1")} forward w.r.t right") =
    forAll(genArgN1_ArgV1_ForSpecBase) { case (a: V1, b: Var1) =>
      op(a, b).forward[V1, V2](b) shouldCloseTo diag(rightDeriv(a, b))
  }

  property(s"${op("var1", "var1")} forward w.r.t self") =
    forAll(genArgV1ForSpecBase) { (a: Var1) =>
      op(a, a).forward[V1, V2](a) shouldCloseTo diag(leftRightDeriv(a))
  }

  property(s"${op("nonvar1", "nonvar1")} reverse node0") =
    forAll(genArgNV1_ArgNV1_ForSpecBase, genAdjointN0ForSpecBase) { case ((a: V1, b: V1), c: V0) =>
      op(a, b).reverse(c).size == 0
  }

  property(s"${op("nonvar1", "nonvar1")} reverse node1") =
    forAll(genArgNV1_ArgNV1_N1_ForSpecBase) { case (a: V1, b: V1, c: V1) =>
      op(a, b).reverse(c).size == 0
  }

  property(s"${op("nonvar1", "nonvar1")} reverse node2") =
    forAll(genArgNV1_ArgNV1_N2_ForSpecBase) { case (a: V1, b: V1, c: V2) =>
      op(a, b).reverse(c).size == 0
  }

  property(s"${op("var1", "nonvar1")} reverse node0") =
    forAll(genArgV1_ArgNV1_ForSpecBase, genAdjointN0ForSpecBase) { case ((a: Var1, b: V1), c: V0) =>
      val g = op(a, b).reverse(c)
      g(a).get.asInstanceOf[V1] shouldCloseTo expectReverseLeft0(a, b, c)
  }

  property(s"${op("var1", "nonvar1")} reverse node1") =
    forAll(genArgV1_ArgNV1_N1_ForSpecBase) { case (a: Var1, b: V1, c: V1) =>
      val g = op(a, b).reverse(c)
      g(a).get.asInstanceOf[V1] shouldCloseTo expectReverseLeft1(a, b, c)
  }

  property(s"${op("var1", "nonvar1")} reverse node2") =
    forAll(genArgV1_ArgNV1_N2_ForSpecBase) { case (a: Var1, b: V1, c: V2) =>
      val g = op(a, b).reverse(c)
      g(a).get.asInstanceOf[V2] shouldCloseTo expectReverseLeft2(a, b, c)
  }

  property(s"${op("nonvar1", "var1")} reverse node0") =
    forAll(genArgNV1_ArgV1_ForSpecBase, genAdjointN0ForSpecBase) { case ((a: V1, b: Var1), c: V0) =>
      val g = op(a, b).reverse(c)
      g(b).get.asInstanceOf[V1] shouldCloseTo expectReverseRight0(a, b, c)
  }

  property(s"${op("nonvar1", "var1")} reverse node1") =
    forAll(genArgNV1_ArgV1_N1_ForSpecBase) { case (a: V1, b: Var1, c: V1) =>
      val g = op(a, b).reverse(c)
      g(b).get.asInstanceOf[V1] shouldCloseTo expectReverseRight1(a, b, c)
  }

  property(s"${op("nonvar1", "var1")} reverse node2") =
    forAll(genArgNV1_ArgV1_N2_ForSpecBase) { case (a: V1, b: Var1, c: V2) =>
      val g = op(a, b).reverse(c)
      g(b).get.asInstanceOf[V2] shouldCloseTo expectReverseRight2(a, b, c)
  }

  property(s"${op("var1", "var1")} reverse node0") =
    forAll(genArgV1ForSpecBase, genAdjointN0ForSpecBase) { (a: Var1, b: V0) =>
      val g = op(a, a).reverse(b)
      g(a).get.asInstanceOf[V1] shouldCloseTo expectReverseLeftRight0(a, b)
  }

  property(s"${op("var1", "var1")} reverse node1") =
    forAll(genArgV1_N1_ForSpecBase) { case (a: Var1, b: V1) =>
      val g = op(a, a).reverse(b)
      g(a).get.asInstanceOf[V1] shouldCloseTo expectReverseLeftRight1(a, b)
  }

  property(s"${op("var1", "var1")} reverse node2") =
    forAll(genArgV1_N2_ForSpecBase) { case (a: Var1, b: V2) =>
      val g = op(a, a).reverse(b)
      g(a).get.asInstanceOf[V2] shouldCloseTo expectReverseLeftRight2(a, b)
  }
}
