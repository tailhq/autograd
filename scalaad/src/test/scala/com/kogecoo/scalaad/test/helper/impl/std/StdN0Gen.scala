package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.StdScalar
import com.kogecoo.scalaad.test.helper._
import org.scalacheck.Gen
import shapeless.Nat._0


class StdN0Gen extends N0Gen[Double] {

  override def genVar0(valueGen: Gen[Double]): Gen[Var[_0]] = {
    for (v <- valueGen) yield Var[_0](StdScalar(v))
  }

  override def genConst0(valueGen: Gen[Double]): Gen[Const[_0]] = {
    for (v <- valueGen) yield Const[_0](StdScalar(v))
  }

}

