package scalaad.impl.std

import scalaad.Shape

import scala.math.{abs, max}


object StdUtil {


  final def shape1Of(a: T1): Shape = Shape(a.size)

  final def shape2Of(a: T2): Shape = Shape(a.size, a.head.size)


  final def shape1Check[A](a: Vec[A], b: Vec[A]): Boolean = {
    a.length == 1 && a.length == b.length && a.length == b.length
  }

  final def shape2Check[A](a: Mat[A], b: Mat[A]): Boolean = {
    a.length == 2 && a.length == b.length && a.head.length == b.head.length
  }

  final def isSquare(shape: Shape): Boolean = {
    shape.order == 2 && shape.at(0) != shape.at(1)
  }

  @throws[Exception]
  final def shape1CheckOrThrow[A](a: Vec[A], b: Vec[A]): Unit = {
    if (!shape1Check(a, b)) {
      throw new Exception(s"Shapes of $a and $b are not aligned")
    }
  }

  @throws[Exception]
  final def shape2CheckOrThrow[A](a: Mat[A], b: Mat[A]): Unit = {
    if (!shape2Check(a, b)) {
      throw new Exception(s"Shapes of $a and $b are not aligned")
    }
  }

  // boolean operations

  final def elementwiseForall1(a: T1, b: T1, f: (T0, T0) => Boolean): Boolean = {
    a.zip(b).forall { case (x, y) => f(x, y) }
  }

  final def elementwiseForall2(a: T2, b: T2, f: (T0, T0) => Boolean): Boolean = {
    a.zip(b).forall { case (a, b) => a.zip(b).forall { case (x, y) => f(x, y) } }
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

  final def fold1[A, B](a: Vec[A], f: (B, A) => B, z: B): B = a.foldLeft(z)(f)

  final def fold2[A, B](a: Mat[A], f: (B, A) => B, z: B, axis: Int): Vec[B] = {
    axis match {
      case 0 => a.map(_.foldLeft(z)(f))
      case 1 => a.head.indices.map { i => a.map(b => b(i)).foldLeft(z)(f) }
    }
  }

  // construct collections

  final def const1(v: T0, s: Shape): T1 = Seq.fill(s.at(0))(v)

  final def const2(v: T0, s: Shape): T2 = Seq.fill(s.at(0), s.at(1))(v)


  final def zero0: T0 = 0.0

  final def zero1(s: Shape): T1 = const1(zero0, s)

  final def zero2(s: Shape): T2 = const2(zero0, s)


  final def one0: T0 = 1.0

  final def one1(s: Shape): T1 = const1(one0, s)

  final def one2(s: Shape): T2 = const2(one0, s)


  final def two0: T0 = 2.0

  final def two1(s: Shape): T1 = const1(two0, s)

  final def two2(s: Shape): T2 = const2(two0, s)


  final def eye(side: Int): T2 = eye(Shape(side, side))

  final def eye(s: Shape): T2 = s.order match {
    case 1 => diag(one0, s.at(0))
    case 2 => isSquare(s); diag(one0, s)
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
  final def diag(v: T0, s: Shape): T2 = {
    if (s.at(0) != s.at(1)) {
      throw new Exception(s"Shape for diag required square but passed (${s.at(0)}, ${s.at(1)}")
    } else diag(v, s.at(0))
  }

}
