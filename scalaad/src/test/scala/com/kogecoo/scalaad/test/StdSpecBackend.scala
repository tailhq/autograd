package com.kogecoo.scalaad.test

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.impl.std
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.impl.std.StdUtil
import com.kogecoo.scalaad.test.helper.impl.std.{StdN0Gen, StdN1Gen, StdN2Gen, StdValueGen}
import com.kogecoo.scalaad.test.helper.{N0Gen, N1Gen, N2Gen}
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{Gen, Prop}
import shapeless.Nat.{_1, _2}


trait StdSpecBackend extends SpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override final type T0 = std.T0

  override final type T1 = std.T1

  override final type T2 = std.T2


  override final def shape1Of(a: T1): Shape[_1] = StdUtil.shape1Of(a)

  override final def shape2Of(a: T2): Shape[_2] = StdUtil.shape2Of(a)


  override final def one0: T0 = 1.0

  override final def zero0: T0 = 0.0

  override final def const1(v: T0, s1: Shape[_1]): T1 = StdUtil.const1(v, s1)

  override final def const2(v: T0, s2: Shape[_2]): T2 = StdUtil.const2(v, s2)


  override final def diag(v: T0, size: Int): T2 = StdUtil.diag(v, size)

  override final def diag(v: T1): T2 = StdUtil.diag(v)

  override final def diag(v: T0, s: Shape[_2]): T2 = StdUtil.diag(v, s)


  override final def broadcast1(a: T1, f: T0 => T0): T1 = StdUtil.broadcast1(a, f)

  override final def broadcast2(a: T2, f: T0 => T0): T2 = StdUtil.broadcast2(a, f)


  override final def elementwise1(a: T1, b: T1, f: (T0, T0) => T0): T1 = StdUtil.elementwise1(a, b, f)

  override final def elementwise2(a: T2, b: T2, f: (T0, T0) => T0): T2 = StdUtil.elementwise2(a, b, f)


  override final def rowwise(a: T2, b: T1, f: (T0, T0) => T0): T2 = StdUtil.rowwise(a, b, f)

  override final def columnwise(a: T2, b: T1, f: (T0, T0) => T0): T2 = StdUtil.columnwise(a, b, f)


  override final def add(a: T0, b: T0): T0 = a + b

  override final def mul(a: T0, b: T0): T0 = a * b

  override final def div(a: T0, b: T0): T0 = a / b

  override def dot(a: T1, b: T1): T0 = elementwise1(a, b, mul).sum

  override def matmul(a: T2, b: T2): T2 = {
    //assert(a.head.size == b.size)
    Seq.range(0, StdUtil.shape2Of(b).at(1)).map { rcolIndex =>
      a.map { lrow =>
        lrow.zip(b.map(_.apply(rcolIndex))).map({ case (x, y) => x * y }).sum
      }
    }
  }

  override final def sum1(a: T1): T0 = a.sum

  override final def sum2(a: T2): T0 = a.map(_.sum).sum


  override final def n0gen: N0Gen[T0] = new StdN0Gen()

  override final def n1gen: N1Gen[T0] = new StdN1Gen()

  override final def n2gen: N2Gen[T0] = new StdN2Gen()

  override final def genValue(maybeMin: Option[T0], maybeMax: Option[T0], constraint: T0 => Boolean): Gen[T0] = {
    StdValueGen(maybeMin, maybeMax, constraint)
  }


  override final def toT0: SpecBackendHelper.ToT0[T0] = SpecBackendHelper.ToT0(_.eval[T0])

  //override final def toT1: SpecBackendHelper.ToT1[T1] = SpecBackendHelper.ToT1(_.eval[T1])

  //override final def toT2: SpecBackendHelper.ToT2[T2] = SpecBackendHelper.ToT2(_.eval[T2])

  override final def eps: SpecBackendHelper.Eps[T0] = SpecBackendHelper.Eps(1e-5)


  override final def shouldEqualTo(a: T0, b: T0): Prop = {
    (a == b) :| s"$a \n  should equal to \n    $b"
  }

  override final def shouldEqualTo(a: T1, b: T1)(implicit d: DummyImplicit): Prop = {
    (shape1Check(shape1Of(a), shape1Of(b)) && a.equalTo(b)) :| s"$a \n  should equal to \n    $b"
  }

  override final def shouldEqualTo(a: T2, b: T2)(implicit d1: DummyImplicit, d2: DummyImplicit): Prop = {
    (shape2Check(shape2Of(a), shape2Of(b)) && a.equalTo(b)) :| s"$a \n  should equal to \n    $b"
  }

  override final def shouldCloseTo(a: T0, b: T0, relDiff: T0): Prop = {
    closeTo(a, b, relDiff) :| s"$a \n  should close to \n    $b"
  }

  override final def shouldCloseTo(a: T1, b: T1, relDiff: T0)(implicit d: DummyImplicit): Prop = {
    (shape1Check(shape1Of(a), shape1Of(b)) && a.closeTo(b, relDiff)) :| s"$a \n  should close to \n    $b"
  }

  override final def shouldCloseTo(a: T2, b: T2, relDiff: T0)(implicit d1: DummyImplicit, d2: DummyImplicit): Prop = {
    (shape2Check(shape2Of(a), shape2Of(b)) && a.closeTo(b, relDiff)) :| s"$a \n  should close to \n    $b"
  }

  private[this] final def closeTo(a: T0, b: T0, relDiff: T0 = eps.value): Boolean = {
    a.closeTo(b, relDiff)
  }

}
