package com.kogecoo.scalaad.impl.breeze


import breeze.linalg.DenseVector
import com.kogecoo.scalaad.impl.std.StdVec
import com.kogecoo.scalaad.{S1, Shape1, Tensor1}


case class BreezeVector(data: DenseVector[Double], transposed: Boolean = false) extends Tensor1 {
  def toStdFloat: StdVec[Float] = toStdDouble.map(_.toFloat)
  def toStdDouble: StdVec[Double] = data.toArray.toSeq

  override def shape: S1 = Shape1(data.size)
}

