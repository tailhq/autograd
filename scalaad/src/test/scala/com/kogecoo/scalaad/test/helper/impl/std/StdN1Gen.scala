package scalaad.test.helper.impl.std

import scalaad.Shape
import scalaad.graph._
import scalaad.impl.std.{StdVector, T0}
import scalaad.test.helper._
import org.scalacheck.Gen


class StdN1Gen extends N1Gen[Double] {

  override def genVar1(shapeGen: Gen[Shape], valueGen: Gen[Double]): Gen[Var] = {
    for {
      s      <- shapeGen
      vec    <- Gen.listOfN(s.at(0), valueGen)
      stdVec =  StdVector[T0, Real](vec)
    } yield Var(stdVec)
  }

  override def genConst1(shapeGen: Gen[Shape], valueGen: Gen[Double]): Gen[Const[Real]] = {
    for {
      s      <- shapeGen
      vec    <- Gen.listOfN(s.at(0), valueGen)
      stdVec =  StdVector[T0, Real](vec)
    } yield Const(stdVec)
  }

}
