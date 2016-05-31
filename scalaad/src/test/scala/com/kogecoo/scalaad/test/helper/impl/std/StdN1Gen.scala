package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.S1
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.StdVector
import com.kogecoo.scalaad.test.helper._
import org.scalacheck.Gen


class StdN1Gen extends N1Gen[Double] {

  override def genVar1(shapeGen: Gen[S1], valueGen: Gen[Double]): Gen[Var1] = {
    for {
      s      <- shapeGen
      vec    <- Gen.listOfN(s._1, valueGen)
      stdVec =  StdVector(vec/*, s.transposed*/)
    } yield Var1(stdVec, s)
  }

  override def genConst1(shapeGen: Gen[S1], valueGen: Gen[Double]): Gen[Const1] = {
    for {
      s      <- shapeGen
      vec    <- Gen.listOfN(s._1, valueGen)
      stdVec =  StdVector(vec/*, s.transposed*/)
    } yield Const1(stdVec, s)
  }

}
