package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{V0, V1, V2, Var1}
import com.kogecoo.scalaad.test.{NodeSpecBase, SpecBackend}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


trait UnaryOp1SpecBase extends NodeSpecBase { self: Properties with SpecBackend =>

  import com.kogecoo.scalaad.test.SpecBackendHelper.Implicits._

  def op(a: String): String

  def op(a: V1): V1


  def expectApplyOp(a: V1): T1

  def deriv(a: T0): T0

  def expectReverse0(a: Var1, b: V0): T1 = broadcast1(broadcast1(a.toT1, deriv), mul(_, b.toT0))

  def expectReverse1(a: Var1, b: V1): T1 = elementwise1(broadcast1(a.toT1, deriv), b.toT1, mul(_, _))

  def expectReverse2(a: Var1, b: V2): T2 = columnwise(b.toT2, broadcast1(a.toT1, deriv), mul(_, _))


  def genArgN1ForSpecBase: Gen[V1] = genN1()

  def genArgNV1ForSpecBase: Gen[V1] = genNV1()

  def genArgV1ForSpecBase: Gen[Var1] = genV1()

  def genN0ForSpecBase: Gen[V0] = genN0()

  def genN1ForSpecBase: Gen[V1] = genN1()

  def genArgNV1_N1_ForSpecBase: Gen[(V1, V1)] = genNV1_N1()

  def genArgNV1_N2_ForSpecBase: Gen[(V1, V2)] = genNV1_RowEquivN2()

  def genArgV1_N1_ForSpecBase: Gen[(Var1, V1)] = genV1_N1()

  def genArgV1_N2_ForSpecBase: Gen[(Var1, V2)] = genV1_RowEquivN2()


  property("eval") = forAll(genArgN1ForSpecBase) { (a: V1) =>
    op(a) shouldCloseTo expectApplyOp(a)
  }

  property(s"${op("node0")} forward w.r.t node0") = forAll(genArgN1ForSpecBase, genN0ForSpecBase) { (a: V1, b: V0) =>
    op(a).forward[V0, V1](b) shouldCloseTo zero1(a)
  }

  property(s"${op("node0")} forward w.r.t node1") = forAll(genArgN1ForSpecBase, genN1ForSpecBase) { (a: V1, b: V1) =>
    op(a).forward[V1, V2](b) shouldCloseTo zero2(a, b)
  }

  property(s"${op("var1")} forward w.r.t self") = forAll(genArgV1ForSpecBase) { (a: Var1) =>
    op(a).forward[V1, V2](a) shouldCloseTo diag(broadcast1(a.toT1, deriv))
  }

  property(s"${op("nonvar0")} reverse node0") = forAll(genArgNV1ForSpecBase, genN0ForSpecBase) { (a: V1, b: V0) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node1") = forAll(genArgNV1_N1_ForSpecBase) { case (a: V1, b: V1) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node2") = forAll(genArgNV1_N2_ForSpecBase) { case (a: V1, b: V2) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("var1")} reverse node0") = forAll(genArgV1ForSpecBase, genN0ForSpecBase) { (a: Var1, b: V0) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[V1] shouldCloseTo expectReverse0(a, b)
  }

  property(s"${op("var1")} reverse node1") = forAll(genArgV1_N1_ForSpecBase) { case (a: Var1, b: V1) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[V1] shouldCloseTo expectReverse1(a, b)
  }

  property(s"${op("var1")} reverse node2") = forAll(genArgV1_N2_ForSpecBase) { case (a: Var1, b: V2) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[V2] shouldCloseTo expectReverse2(a, b)
  }

}
