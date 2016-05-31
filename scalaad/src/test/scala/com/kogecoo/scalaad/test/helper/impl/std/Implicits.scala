package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.impl.std.StdUtil
import com.kogecoo.scalaad.impl.std.StdUtil.{T0, T1, T2}



object Implicits {

  implicit class StdScalarOp(val self: T0) extends AnyVal {

    final def closeTo(rhs: T0, relDiff: T0 = 1e-5): Boolean = StdUtil.closeTo0(self, rhs, relDiff)

  }

  implicit class StdVecOp(val self: T1) extends AnyVal {

    final def broadcast(f: T0 => T0): T1 = StdUtil.broadcast1(self, f)

    final def elementwise(other: T1, f: (T0, T0) => T0): T1 = StdUtil.elementwise1(self, other, f)

    final def equalTo(rhs: T1): Boolean = StdUtil.equalTo1(self, rhs)

    final def closeTo(rhs: T1, relDiff: T0 = 1e-5): Boolean = StdUtil.closeTo1(self, rhs)

  }


  implicit class StdMatOp(val self: T2) extends AnyVal {

    final def broadcast(f: T0 => T0): T2 = self.map(_.map(f))

    final def elementwise(rhs: T2, f: (T0, T0) => T0): T2 = StdUtil.elementwise2(self, rhs, f)

    final def columnwise(rhs: T1, f: (T0, T0) => T0): T2 = StdUtil.columnwise(self, rhs, f)

    final def rowwise(rhs: T1, f: (T0, T0) => T0): T2 = StdUtil.rowwise(self, rhs, f)

    final def equalTo(rhs: T2): Boolean = StdUtil.equalTo2(self, rhs)

    final def closeTo(rhs: T2, relDiff: T0 = 1e-5): Boolean = StdUtil.closeTo2(self, rhs)

  }


  implicit class N0Ops(val self: VE0) extends AnyVal { def toT0: T0 = self.eval[T0] }

  //implicit class N1Ops(val self: V1) extends AnyVal { def toT1: T1 = self.eval[T1] }

  //implicit class N2Ops(val self: V2) extends AnyVal { def toT2: T2 = self.eval[T2] }

}
