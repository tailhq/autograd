package com.kogecoo.scalaad.test

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.test.helper.{N0Gen, N1Gen, N2Gen}
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{Gen, Prop}
import shapeless.Nat.{_1, _2}


trait SpecBackend {

  type T0
  type T1
  type T2


  def shape1Of(a: T1): Shape[_1]

  def shape2Of(a: T2): Shape[_2]


  def one0: T0

  def zero0: T0

  def const1(v: T0, s1: Shape[_1]): T1

  def const2(v: T0, s1: Shape[_2]): T2


  def diag(v: T0, size: Int): T2

  def diag(v: T1): T2

  def diag(v: T0, s: Shape[_2]): T2


  def broadcast1(a: T1, f: T0 => T0): T1

  def broadcast2(a: T2, f: T0 => T0): T2


  def elementwise1(a: T1, b: T1, f: (T0, T0) => T0): T1

  def elementwise2(a: T2, b: T2, f: (T0, T0) => T0): T2

  /**
    *
    *     |a, b, c|
    * x = |d, e, f|  y = [u, v, w]
    *     |g, h, i|
    *
    *                      |a + u, b + v, c + w|
    * rowwise(x, y, add) = |d + u, e + v, f + w|
    *                      |g + u, h + v, i + w|
 *
 *                         |a + u, b + u, c + u|
    * columnwise(x, y, add) = |d + v, e + v, f + v|
    *                         |g + w, h + w, i + w|
    *
    */
  def rowwise(a: T2, b: T1, f: (T0, T0) => T0): T2

  def columnwise(a: T2, b: T1, f: (T0, T0) => T0): T2

  def add(a: T0, b: T0): T0

  def mul(a: T0, b: T0): T0

  def div(a: T0, b: T0): T0

  def dot(a: T1, b: T1): T0

  def matmul(a: T2, b: T2): T2

  def sum1(a: T1): T0

  def sum2(a: T2): T0


  def n0gen: N0Gen[T0]

  def n1gen: N1Gen[T0]

  def n2gen: N2Gen[T0]

  def genValue(maybeMin: Option[T0], maybeMax: Option[T0], constraint: T0 => Boolean): Gen[T0]


  implicit def toT0: SpecBackendHelper.ToT0[T0]

  implicit def toT1: SpecBackendHelper.ToT1[T1]

  implicit def toT2: SpecBackendHelper.ToT2[T2]

  implicit def eps: SpecBackendHelper.Eps[T0]


  implicit def shouldEqualTo(a: T0, b: T0): Prop

  implicit def shouldEqualTo(a: T1, b: T1)(implicit d: DummyImplicit): Prop

  implicit def shouldEqualTo(a: T2, b: T2)(implicit d1: DummyImplicit, d2: DummyImplicit): Prop


  implicit def shouldCloseTo(a: T0, b: T0, relDiff: T0): Prop

  implicit def shouldCloseTo(a: T1, b: T1, relDiff: T0)(implicit d: DummyImplicit): Prop

  implicit def shouldCloseTo(a: T2, b: T2, relDiff: T0)(implicit d1: DummyImplicit, d2: DummyImplicit): Prop


  final def shape1Check(a: Shape[_1], b: Shape[_1]): Boolean = {
    a.at(0) == b.at(0)
  }

  final def shape2Check(a: Shape[_2], b: Shape[_2]): Boolean = {
    a.at(0) == b.at(0) && a.at(1) == b.at(1)
  }

  protected[this] final def shapeCheckProp(a: Shape[_1], b: Shape[_1]): Prop = {
    shape1Check(a, b) :| s"the shape $a should equal to $b"
  }

  protected[this] final def shapeCheckProp(a: Shape[_2], b: Shape[_2])(implicit d: DummyImplicit): Prop = {
    shape2Check(a, b) :| s"the shape $a should equal to $b"
  }

}
