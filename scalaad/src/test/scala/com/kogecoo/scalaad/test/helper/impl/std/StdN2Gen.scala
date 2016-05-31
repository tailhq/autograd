package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.S2
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.{StdMat, StdMatrix}
import com.kogecoo.scalaad.test.helper._
import org.scalacheck.Gen


class StdN2Gen extends N2Gen[Double] {

  private[this] def genStdMatWithShape(shapeGen: Gen[S2], valueGen: Gen[Double]): Gen[StdMat[Double]] = {
    for {
      s    <- shapeGen
      m    <- Gen.listOfN(s._1, Gen.listOfN(s._2, valueGen))
    } yield m

  }

  override def genVar2(shapeGen: Gen[S2], valueGen: Gen[Double]): Gen[Var2] = {
    for {
      s   <- shapeGen
      mat <- genStdMatWithShape(s, valueGen)
    } yield Var2(StdMatrix(mat), s)
  }

  override def genConst2(shapeGen: Gen[S2], valueGen: Gen[Double]): Gen[Const2] = {
    for {
      s   <- shapeGen
      mat <- genStdMatWithShape(s, valueGen)
    } yield Const2(StdMatrix(mat), s)
  }

}
