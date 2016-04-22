package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.StdVector
import com.kogecoo.scalaad.test.helper._
import org.scalacheck.Gen
import shapeless.Nat._1


class StdN1Gen extends N1Gen[Double] {

  override def genVar1(shapeGen: Gen[Shape[_1]], valueGen: Gen[Double]): Gen[Var[_1]] = {
    for {
      s      <- shapeGen
      vec    <- Gen.listOfN(s.at(0), valueGen)
      stdVec =  StdVector(vec)
    } yield Var[_1](stdVec)
  }

  override def genConst1(shapeGen: Gen[Shape[_1]], valueGen: Gen[Double]): Gen[Const[_1]] = {
    for {
      s      <- shapeGen
      vec    <- Gen.listOfN(s.at(0), valueGen)
      stdVec =  StdVector(vec)
    } yield Const[_1](stdVec)
  }

}
