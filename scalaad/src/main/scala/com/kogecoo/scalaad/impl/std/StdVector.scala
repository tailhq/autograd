package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{Shape, Shape1, Tensor1}

import shapeless.Nat._1


case class StdVector(data: StdVec[Double], transposed: Boolean=false) extends Tensor1 {

  def toStdFloat: StdVec[Float] = data.map(_.toFloat)
  def toStdDouble: StdVec[Double] = data

  override def shape: Shape[_1] = Shape1(data.size)

}
