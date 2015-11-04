package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.graph.{S1, S2}
import com.kogecoo.scalaad.{Shape1, Shape2}

import scala.math.{abs, max}


object StdUtil {

  type T0 = Double
  type T1 = StdVec[Double]
  type T2 = StdMat[Double]

  // Matrix is expressed by following
  //              Seq(
  // |1, 2, 3|      Seq(1, 2, 3),
  // |4, 5, 6| =    Seq(4, 5, 6),
  // |7, 8, 9|      Seq(7, 8, 9)
  //              )

  final def shapeOf(a: T1): S1 = Shape1(a.size)
  final def shapeOf(a: T2): S2 = Shape2(a.size, a.head.size)

  final def shapeCheck1(a: T1, b: T1): Boolean = a.size == b.size
  final def shapeCheck2(a: T2, b: T2): Boolean = a.size == b.size && a.head.size == b.head.size

  final def shapeCheckOrThrow1(a: T1, b: T1): Unit = if (!shapeCheck1(a, b)) { throw new Exception(s"Shapes of $a and $b are not aligned") }
  final def shapeCheckOrThrow2(a: T2, b: T2): Unit = if (!shapeCheck2(a, b)) { throw new Exception(s"Shapes of $a and $b are not aligned") }

  // boolean operations
  final def elementwiseForall1(a: T1, b: T1, f: (T0, T0) => Boolean): Boolean = elementwise1(a, b, f).forall(n => n)
  final def elementwiseForall2(a: T2, b: T2, f: (T0, T0) => Boolean): Boolean = elementwise2(a, b, f).map(_.forall(m => m)).forall(n => n)

  // ref: https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
  final def closeTo0(a: T0, b: T0, relDiff: T0 = 1e-5): Boolean = abs(a - b) <= max(abs(a), abs(b)) * relDiff
  final def closeTo1(a: T1, b: T1, relDiff: T0 = 1e-5): Boolean = elementwiseForall1(a, b, (x, y) => closeTo0(x, y, relDiff))
  final def closeTo2(a: T2, b: T2, relDiff: T0 = 1e-5): Boolean = elementwiseForall2(a, b, (x, y) => closeTo0(x, y, relDiff))

  final def equalTo0(a: T0, b: T0): Boolean = a == b
  final def equalTo1(a: T1, b: T1): Boolean = elementwiseForall1(a, b, equalTo0)
  final def equalTo2(a: T2, b: T2): Boolean = elementwiseForall2(a, b, equalTo0)

  // operations for collections
  final def broadcast1[A](a: T1, f: T0 => A): StdVec[A] = a.map(f)
  final def broadcast2[A](a: T2, f: T0 => A): StdMat[A] = a.map(_.map(f))

  final def elementwise1[A](a: T1, b: T1, f: (T0, T0) => A): StdVec[A] = { shapeCheckOrThrow1(a, b); a.zip(b).map { case (x, y) => f(x, y) } }
  final def elementwise2[A](a: T2, b: T2, f: (T0, T0) => A): StdMat[A] = { shapeCheckOrThrow2(a, b); a.zip(b).map { case (x, y) => x.zip(y).map { case (v, w) => f(v, w) } } }

  /**
    * For example:
    *
    *      |a, b|      |e|
    *  x = |c, d|, y = |f|
    *                          |a + e, b + e|
    *  columnwise(x, y, add) = |c + f, d + f|
    *
    * Another example:
    *
    *  v = Seq(          w = Seq(e, f)
    *        Seq(a, b),
    *        Seq(c, d),
    *      )
    *                           Seq(
    *                             Seq(a + e, b + e),
    *  columnwise(v, w, add) =    Seq(c + f, d + f),
    *                           )
   **/
  final def columnwise[A](a: T2, b: T1, f: (T0, T0) => A): StdMat[A] = {
    if (a.size == b.size) {
      a.zip(b).map { case (x, y) => x.map(f(_, y)) }
    } else {
      throw new Exception(
        "columnwise operation cannot be performed because:\n" +
         s" A num of row of Matrix(${a.size}, ${a.head.size}) need to be equal to size of Vector(${b.size})"
      )
    }
  }

  /**
    * For example:
    *
    *      |a, b|      |e|
    *  x = |c, d|, y = |f|
    *                       |a + e, b + f|
    *  rowwise(x, y, add) = |c + e, d + f|
    *
    * Another example:
    *
    *  v = Seq(          w = Seq(e, f)
    *        Seq(a, b),
    *        Seq(c, d),
    *      )
    *                           Seq(
    *                             Seq(a + e, b + f),
    *  rowwise(v, w, add) =       Seq(c + e, d + f),
    *                           )
   **/
  final def rowwise[A](a: T2, b: T1, f: (T0, T0) => A): StdMat[A] = {
    if (a.head.size == b.size) {
      a.map { _.zip(b).map { case (x, y) => f(x, y) }
      }
    } else {
      throw new Exception(
        "rowwise operation cannot be performed because:\n" +
         s" A num of column of Matrix(${a.size}, ${a.head.size}) need to be equal to size of Vector(${b.size})\n" +
        " or num of columns for each rows are not aligned."
      )
    }
  }



  // construct collections
  final def const1(v: T0, s1: S1): T1 = Seq.fill(s1._1)(v)
  final def const2(v: T0, s2: S2): T2 = Seq.fill(s2._1, s2._2)(v)

  final def one0: T0 = 1.0
  final def zero0: T0 = 0.0

  final def diag(v: T0, size: Int): T2 = {
    val s = Seq.range(0, size)
    s.map { i => s.map { j => if (i == j) v else zero0 } }
  }

  final def diag(v: T1): T2 = {
    val s = v.indices
    s.map { i => s.map { j => if (i == j) v(i) else zero0 } }
  }
  final def diag(v: T0, s: Shape2): T2 = {
    if (s._1 != s._2) {
      throw new Exception(s"Shape for diag required square but passed (${s._1}, ${s._2}")
    } else diag(v, s._1)
  }

}
