package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.{Mat, StdMatrix}
import com.kogecoo.scalaad.test.helper._
import org.scalacheck.Gen


class StdN2Gen extends N2Gen[Double] {

  private[this] def genStdMatWithShape(shapeGen: Gen[Shape], valueGen: Gen[Double]): Gen[Mat[Double]] = {
    for {
      s    <- shapeGen
      m    <- Gen.listOfN(s.at(0), Gen.listOfN(s.at(1), valueGen))
    } yield m

  }

  override def genVar2(shapeGen: Gen[Shape], valueGen: Gen[Double]): Gen[Var] = {
    for {
      s   <- shapeGen
      mat <- genStdMatWithShape(s, valueGen)
    } yield Var(StdMatrix(mat))
  }

  override def genConst2(shapeGen: Gen[Shape], valueGen: Gen[Double]): Gen[Const] = {
    for {
      s   <- shapeGen
      mat <- genStdMatWithShape(s, valueGen)
    } yield Const(StdMatrix(mat))
  }

}
