package com.kogecoo.scalaad.test

import com.kogecoo.scalaad.graph.{V0, V1, V2}
import org.scalacheck.Prop


object SpecBackendHelper {

  case class Eps[T](value: T)

  case class ToT0[T0](toT0: V0 => T0)

  case class ToT1[T1](toT1: V1 => T1)

  case class ToT2[T2](toT2: V2 => T2)


  object Implicits {

    implicit class N0Ops[T0](val self: V0) extends AnyVal {

      def toT0(implicit cvt: ToT0[T0]): T0 = cvt.toT0(self)

      def shouldEqualTo(rhs: T0)(implicit cmp: (T0, T0) => Prop, cvt: ToT0[T0]): Prop = cmp(self.toT0, rhs)

      def shouldCloseTo(rhs: T0)(implicit cmp: (T0, T0, T0) => Prop, eps: Eps[T0], cvt: ToT0[T0]): Prop = cmp(self.toT0, rhs, eps.value)

    }

    implicit class N1Ops[T0, T1](val self: V1) extends AnyVal {

      def toT1(implicit cvt: ToT1[T1]): T1 = cvt.toT1(self)

      def shouldEqualTo(rhs: T1)(implicit cmp: (T1, T1) => Prop, cvt: ToT1[T1]): Prop = cmp(self.toT1, rhs)

      def shouldCloseTo(rhs: T1)(implicit cmp: (T1, T1, T0) => Prop, eps: Eps[T0], cvt: ToT1[T1]): Prop = cmp(self.toT1, rhs, eps.value)

    }

    implicit class N2Ops[T0, T2](val self: V2) extends AnyVal {

      def toT2(implicit cvt: ToT2[T2]): T2 = cvt.toT2(self)

      def shouldEqualTo(rhs: T2)(implicit cmp: (T2, T2) => Prop, cvt: ToT2[T2]): Prop = cmp(self.toT2, rhs)

      def shouldCloseTo(rhs: T2)(implicit cmp: (T2, T2, T0) => Prop, eps: Eps[T0], cvt: ToT2[T2]): Prop = cmp(self.toT2, rhs, eps.value)

    }

  }
}
