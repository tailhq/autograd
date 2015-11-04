package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.impl.std.{StdUtil, T0, T1, T2}



object Implicits {

  implicit class StdScalarOp(val self: T0) extends AnyVal {

    final def closeTo(rhs: T0, relDiff: T0 = 1e-5): Boolean = StdUtil.closeTo0(self, rhs, relDiff)

  }

  implicit class StdVecOp(val self: T1) extends AnyVal {

    final def equalTo(rhs: T1): Boolean = StdUtil.equalTo1(self, rhs)

    final def closeTo(rhs: T1, relDiff: T0 = 1e-5): Boolean = StdUtil.closeTo1(self, rhs)

  }


  implicit class StdMatOp(val self: T2) extends AnyVal {

    final def equalTo(rhs: T2): Boolean = StdUtil.equalTo2(self, rhs)

    final def closeTo(rhs: T2, relDiff: T0 = 1e-5): Boolean = StdUtil.closeTo2(self, rhs)

  }


  implicit class N0Ops(val self: V) extends AnyVal { def toT0: T0 = self.eval[T0] }

  implicit class N1Ops(val self: V) extends AnyVal { def toT1: T1 = self.eval[T1] }

  implicit class N2Ops(val self: V) extends AnyVal { def toT2: T2 = self.eval[T2] }

}
