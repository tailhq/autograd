package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{N0, N1, N2, Var2}
import com.kogecoo.scalaad.test.{NodeSpecBase, SpecBackend}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


trait UnaryOp2SpecBase extends NodeSpecBase { self: Properties with SpecBackend =>

  import com.kogecoo.scalaad.test.SpecBackendHelper.Implicits._

  def op(a: String): String

  def op(a: N2): N2


  def deriv(a: T0): T0

  def expectApplyOp(a: N2): T2

  def expectReverse0(a: Var2, b: N0): T2 = broadcast2(broadcast2(a.toT2, deriv), mul(_, b.toT0))

  def expectReverse1(a: Var2, b: N1): T2 = columnwise(broadcast2(a.toT2, deriv), b.toT1, mul)

  def expectReverse2(a: Var2, b: N2): T2 = elementwise2(b.toT2, broadcast2(a.toT2, deriv), mul)


  def genArgN2ForSpecBase: Gen[N2] = genN2()

  def genArgNV2ForSpecBase: Gen[N2] = genNV2()

  def genArgV2ForSpecBase: Gen[Var2] = genV2()

  def genN0ForSpecBase: Gen[N0] = genN0()

  def genArgNV2_N1_ForSpecBase: Gen[(N2, N1)] = genNV2_N1()

  def genArgNV2_N2_ForSpecBase: Gen[(N2, N2)] = genNV2_N2()

  def genArgV2_N1_ForSpecBase: Gen[(Var2, N1)] = genV2_N1()

  def genArgV2_N2_ForSpecBase: Gen[(Var2, N2)] = genV2_N2()

  property("eval") = forAll(genArgN2ForSpecBase) { (a: N2) =>
    op(a) shouldCloseTo expectApplyOp(a)
  }

  property(s"${op("node2")} forward w.r.t node0") = forAll(genArgN2ForSpecBase, genN0ForSpecBase) { (a: N2, b: N0) =>
    op(a).forward[N0, N2](b) shouldCloseTo zero2(a)
  }

  property(s"${op("nonvar2")} reverse node0") = forAll(genArgNV2ForSpecBase, genN0ForSpecBase) { (a: N2, b: N0) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar2")} reverse node1") = forAll(genArgNV2_N1_ForSpecBase) { case (a: N2, b: N1) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar2")} reverse node2") = forAll(genArgNV2_N2_ForSpecBase) { case (a: N2, b: N2) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("var2")} reverse node0") = forAll(genArgV2ForSpecBase, genN0()) { (a: Var2, b: N0) =>
    val g = op(a).reverse(b)
    val y = broadcast2(a.toT2, (x: T0) => deriv(x))
    g(a).get.asInstanceOf[N2] shouldCloseTo broadcast2(y, (z: T0) => mul(z, b.toT0))
  }

  property(s"${op("var2")} reverse node1") = forAll(genArgV2_N1_ForSpecBase) { case (a: Var2, b: N1) =>
    val g = op(a).reverse(b)
    val y = broadcast2(a.toT2, deriv)
    g(a).get.asInstanceOf[N2] shouldCloseTo columnwise(y, b.toT1, mul)
  }

  property(s"${op("var2")} reverse node2") = forAll(genArgV2_N2_ForSpecBase) { case (a: Var2, b: N2) =>
    val g = op(a).reverse(b)
    val y = broadcast2(a.toT2, (x: T0) => deriv(x))
    g(a).get.asInstanceOf[N2] shouldCloseTo elementwise2(y, b.toT2, (v: T0, w: T0) => mul(v, w))
  }

}
