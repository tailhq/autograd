package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.Shape2
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.NodeSpecBase
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

/*
object MatMul22Spec extends Properties("MatMul22") with NodeSpecBase {

  override val defaultMinValue = Some(-1000.0)
  override val defaultMaxValue = Some(1000.0)

  def genS2ColFix(col: Int) = S2Gen(
    defaultS2RowConstraint,
    col,
    defaultS2ShapeConstraint,
    defaultS2ColMin,
    defaultS2ColMax
  )

  private[this] val N2_N2: Gen[(N2, N2)] = for {
    first  <- genN2()
    s2     <- genS2(first.shape._2)
    second <- genN2(s2)
  } yield (first, second)

  private[this] val NV2_NV2_N2: Gen[(N2, N2, N2)] = for {
    first  <- genNV2()
    s2     <- genS2(first.shape._2)
    second <- genNV2(s2)
    third  <- genNV2(Shape2(second.shape._1, first.shape._2))
  } yield (first, second, third)

  private[this] val V2_NV2_N2: Gen[(Var2, N2, N2)] = for {
    first  <- genV2()
    s2     <- genS2(first.shape._2)
    second <- genNV2(s2)
    third  <- genNV2(Shape2(second.shape._1, first.shape._2))
  } yield (first, second, third)

  private[this] val NV2_V2_N2: Gen[(N2,  Var2, N2)] = for {
    first  <- genNV2()
    s2     <- genS2(first.shape._2)
    second <- genV2(s2)
    third  <- genNV2(Shape2(second.shape._1, first.shape._2))
  } yield (first, second, third)

  private[this] val V2_V2_N2: Gen[(Var2,  Var2, N2)] = for {
    s1     <- genS1()
    s2     =  Shape2(s1._1, s1._1)
    first  <- genV2(s2)
    second <- genV2(s2)
    third  <- genNV2(s2)
  } yield (first, second, third)

  property("eval") = forAll(N2_N2) { case (a: N2, b: N2) =>
    MatMul22(a, b).eval[T2] shouldCloseTo (a.toT2 matmul b.toT2)
  }

  property("MatMul(node2, node2) forward w.r.t node0") = forAll(N2_N2, genN0()) { case ((a, b), c) =>
    val expect = Zero2(new Shape2(a.shape._1, b.shape._2)).eval[T2]
    MatMul22(a, b).forward[N0, N2](c).eval[T2] shouldCloseTo zero2(Shape2(a.shape._1, b.shape._2))
  }

  property("MatMul(nonvar2, nonvar2) reverse node2") = forAll(NV2_NV2_N2) { case (a, b, c) =>
    MatMul22(a, b).reverse(c).size == 0
  }

  property("MatMul(var2, nonvar2) reverse node2") = forAll(V2_NV2_N2) { case (a, b, c) =>
    val l = c.toT2 matmul b.toT2
    val r = a.toT2 matmul c.toT2
    val expect = l.zip(r).map { case (x, y) => x.zip(y).map { case (i, j) => i + j } }

    val g = MatMul22(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo expect
  }

  property("MatMul(nonvar2, var2) reverse node2") = forAll(NV2_V2_N2) { case (a, b, c) =>
    val expect = a.toT2 matmul c.toT2

    val g = MatMul22(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo expect
  }

  property("MatMul(var2, var2) reverse node2") = forAll(V2_V2_N2) { case (a, b, c) =>
    val g = MatMul22(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo c.toT2.map(_.map(_ * 2))
  }
}
*/
