package com.kogecoo.scalaad.impl.breeze

import breeze.linalg.BitVector
import com.kogecoo.scalaad.impl.std.StdVec
import com.kogecoo.scalaad.{BooleanTensor1, S1, Shape1}


case class BreezeBooleanVector(data: BitVector) extends BooleanTensor1 {

  override def toStd: StdVec[Boolean] = data.toArray

  override def shape: S1 = Shape1(data.size)

}