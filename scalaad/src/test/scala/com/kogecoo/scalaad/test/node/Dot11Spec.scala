package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.op.Dot11
import com.kogecoo.scalaad.test.{NodeSpecBase, SpecBackend, StdSpecBackend}
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object StdDot11Spec extends Properties("Dotl11") with Dot11Spec with StdSpecBackend {

  override def defaultMinValue = Some(-1e100)
  override def defaultMaxValue = Some(1e100)

  override def expectedApplyOp(a: N1, b: N1): T0 = dot(a.toT1, b.toT1)
}


trait Dot11Spec extends NodeSpecBase { self: Properties with SpecBackend =>

  import com.kogecoo.scalaad.test.SpecBackendHelper.Implicits._

  def op(a: N1, b: N1): N0 = Dot11(a, b)

  def op(a: String, b: String): String = s"$a dot $b"

  def vec(a: N0, s: S1): N1 = VecFill(a, s)

  def expectedApplyOp(a: N1, b: N1): T0

  property("eval") = forAll(genN1_N1()) { case (a: N1, b: N1) =>
    op(a, b) shouldCloseTo expectedApplyOp(a, b)
  }

  property(s"${op("node1", "node1")} forward w.r.t node0") = forAll(genN1_N1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    op(a, b).forward[N0, N0](c) shouldCloseTo zero0
  }

  property(s"${op("vec(var0)", "node1")} forward w.r.t left") = forAll(genV0(), genN1()) { case (a: Var0, b: N1) =>
    op(vec(a, b.shape), b).forward[N0, N0](a) shouldCloseTo sum1(b.toT1)
  }

  property(s"${op("node1", "vec(var0)")} forward w.r.t right") = forAll(genN1(), genV0()) { case (a: N1, b: Var0) =>
    op(a, vec(b, a.shape)).forward[N0, N0](b) shouldCloseTo sum1(a.toT1)
  }

  property(s"${op("vec(var0)", "vec(var0)")} forward w.r.t self") = forAll(genV0(), genS1()) { case (a: Var0, s: S1) =>
    val side = sum1(vec(a, s).toT1)
    op(vec(a, s), vec(a, s)).forward[N0, N0](a) shouldCloseTo add(side, side)
  }

  property(s"${op("nonvar1", "nonvar1")} reverse node0") = forAll(genNV1_NV1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    op(a, b).reverse(c).size == 0
  }

  property(s"${op("nonvar1", "nonvar1")} reverse node1") = forAll(genNV1_NV1(), genN1()) { case ((a: N1, b: N1), c: N1) =>
    op(a, b).reverse(c).size == 0
  }

  property(s"${op("nonvar1", "nonvar1")} reverse node2") = forAll(genNV1_NV1(), genN2()) { case ((a: N1, b: N1), c: N2) =>
    op(a, b).reverse(c).size == 0
  }

  property(s"${op("var1", "nonvar1")} reverse node0") = forAll(genV1_NV1(), genN0()) { case ((a: Var1, b: N1), c: N0) =>
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N1] shouldCloseTo const1(mul(sum1(b.toT1), c.toT0), a.shape)
  }

  property(s"${op("var1", "nonvar1")} reverse node1") = forAll(genV1_NV1_N1()) { case(a: Var1, b: N1, c: N1) =>
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N1] shouldCloseTo const1(dot(a.toT1, b.toT1), a.shape)
  }

  property(s"${op("var1", "nonvar1")} reverse node2") = forAll(genV1_NV1_RowEquivN2()) { case (a: Var1, b: N1, c: N2) =>
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N2] shouldCloseTo columnwise(c.toT2, b.toT1, mul)
  }
/*
  property(s"${op("nonvar1","var1")} reverse node0") = forAll(genNV1_V1(), genN0()) { case ((a: N1, b: Var1), c: N0) =>
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo const1(a.toT1.sum * c.toT0, a.shape)
  }

  property("nonvar1 dot var1 reverse node1") = forAll(genNV1_V1_N1()) { case (a: N1, b: Var1, c: N1) =>
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo const1(a.toT1 dot c.toT1, a.shape)
  }

  property("nonvar1 dot var1 reverse node2") = forAll(genNV1_V1_RowEquivN2()) { case (a: N1, b: Var1, c: N2) =>
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo (c.toStd colMul a.toStd)
  }

  property("var1 dot var1 reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo const1(a.toT1.sum * b.toT0 * 2.0, a.shape)
  }

  property("var1 dot var1 reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo const1((a.toT1 dot b.toT1) * 2.0, a.shape)
  }

  property("var1 dot var1 reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd colMul (a.toStd mul 2))
  }
*/
}

