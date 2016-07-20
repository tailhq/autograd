package scalaad.test.helper.impl.std

import scalaad.graph._
import scalaad.impl.std.StdScalar
import scalaad.test.helper._
import org.scalacheck.Gen


class StdN0Gen extends N0Gen[Double] {

  override def genVar0(valueGen: Gen[Double]): Gen[Var] = {
    for (v <- valueGen) yield Var(StdScalar(v))
  }

  override def genConst0(valueGen: Gen[Double]): Gen[Const] = {
    for (v <- valueGen) yield Const(StdScalar(v))
  }

}

