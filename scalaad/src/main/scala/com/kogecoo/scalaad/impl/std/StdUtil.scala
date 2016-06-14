package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{Shape, Shape1, Shape2}
import shapeless.Nat.{_1, _2}

import scala.math.{abs, max}


object StdUtil {

  // Matrix is expressed by following
  //              Seq(
  // |1, 2, 3|      Seq(1, 2, 3),
  // |4, 5, 6| =    Seq(4, 5, 6),
  // |7, 8, 9|      Seq(7, 8, 9)
  //              )

  final def shape1Of(a: T1): Shape[_1] = Shape1(a.size)

  final def shape2Of(a: T2): Shape[_2] = Shape2(a.size, a.head.size)


  final def shape1Check(a: T1, b: T1): Boolean = a.size == b.size

  final def shape2Check(a: T2, b: T2): Boolean = {
    a.size == b.size && a.head.size == b.head.size
  }

  final def isSquare(shape: Shape[_2]): Boolean = {
    shape.at(0) != shape.at(1)
  }

  @throws[Exception]
  final def shape1CheckOrThrow(a: T1, b: T1): Unit = {
    if (!shape1Check(a, b)) {
      throw new Exception(s"Shapes of $a and $b are not aligned")
    }
  }

  @throws[Exception]
  final def shape2CheckOrThrow(a: T2, b: T2): Unit = {
    if (!shape2Check(a, b)) {
      throw new Exception(s"Shapes of $a and $b are not aligned")
    }
  }


  // boolean operations

  final def elementwiseForall1(a: T1, b: T1, f: (T0, T0) => Boolean): Boolean = {
    elementwise1(a, b, f).forall(n => n)
  }

  final def elementwiseForall2(a: T2, b: T2, f: (T0, T0) => Boolean): Boolean = {
    elementwise2(a, b, f).map(_.forall(m => m)).forall(n => n)
  }

  // ref: https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
  final def closeTo0(a: T0, b: T0, relDiff: T0 = 1e-5f): Boolean = {
    abs(a - b) <= max(abs(a), abs(b)) * relDiff
  }

  final def closeTo1(a: T1, b: T1, relDiff: T0 = 1e-5f): Boolean = {
    elementwiseForall1(a, b, (x: T0, y: T0) => closeTo0(x, y, relDiff))
  }

  final def closeTo2(a: T2, b: T2, relDiff: T0 = 1e-5f): Boolean = {
    elementwiseForall2(a, b, (x: T0, y: T0) => closeTo0(x, y, relDiff))
  }


  final def equalTo0(a: T0, b: T0): Boolean = {
    a == b
  }

  final def equalTo1(a: T1, b: T1): Boolean = {
    elementwiseForall1(a, b, equalTo0)
  }

  final def equalTo2(a: T2, b: T2): Boolean = {
    elementwiseForall2(a, b, equalTo0)
  }


  // operations for collections

  final def broadcast1[A](a: T1, f: T0 => A): Vec[A] = a.map(f)

  final def broadcast2[A](a: T2, f: T0 => A): Mat[A] = a.map(_.map(f))


  final def elementwise1[A](a: T1, b: T1, f: (T0, T0) => A): Vec[A] = {
    shape1CheckOrThrow(a, b)
    a.zip(b).map { case (x, y) => f(x, y) }
  }

  final def elementwise2[A](a: T2, b: T2, f: (T0, T0) => A): Mat[A] = {
    shape2CheckOrThrow(a, b)
    a.zip(b).map { case (x, y) => x.zip(y).map { case (v, w) => f(v, w) } }
  }

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
  final def columnwise[A](a: T2, b: T1, f: (T0, T0) => A): Mat[A] = {
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
  final def rowwise[A](a: T2, b: T1, f: (T0, T0) => A): Mat[A] = {
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

  final def const1(v: T0, s: Shape[_1]): T1 = Seq.fill(s.at(0))(v)

  final def const2(v: T0, s: Shape[_2]): T2 = Seq.fill(s.at(0), s.at(1))(v)


  final def zero0: T0 = 0.0

  final def zero1(s: Shape[_1]): T1 = const1(zero0, s)

  final def zero2(s: Shape[_2]): T2 = const2(zero0, s)


  final def one0: T0 = 1.0

  final def one1(s: Shape[_1]): T1 = const1(one0, s)

  final def one2(s: Shape[_2]): T2 = const2(one0, s)


  final def two0: T0 = 2.0

  final def two1(s: Shape[_1]): T1 = const1(two0, s)

  final def two2(s: Shape[_2]): T2 = const2(two0, s)


  final def eye(side: Int): T2 = eye(Shape2(side, side))

  final def eye(s: Shape[_1]): T2 = diag(one0, s.at(0))

  final def eye(s: Shape[_2])(implicit d: DummyImplicit): T2 = {
    isSquare(s); diag(one0, s)
  }


  final def diag(v: T0, side: Int): T2 = {
    val s = Seq.range(0, side)
    s.map { i => s.map { j => if (i == j) v else zero0 } }
  }

  final def diag(v: T1): T2 = {
    val s = v.indices
    s.map { i => s.map { j => if (i == j) v(i) else zero0 } }
  }

  @throws[Exception]
  final def diag(v: T0, s: Shape[_2]): T2 = {
    if (s.at(0) != s.at(1)) {
      throw new Exception(s"Shape for diag required square but passed (${s.at(0)}, ${s.at(1)}")
    } else diag(v, s.at(0))
  }

}
