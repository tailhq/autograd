package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.StdScalar
import com.kogecoo.scalaad.test.helper._
import org.scalacheck.Gen


class StdN0Gen extends N0Gen[Double] {

  override def genVar0(valueGen: Gen[Double]): Gen[Var0] = {
    for (v <- valueGen) yield Var0(StdScalar(v))
  }

  override def genConst0(valueGen: Gen[Double]): Gen[Const0] = {
    for (v <- valueGen) yield Const0(StdScalar(v))
  }

}

