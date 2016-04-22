package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{Shape, Shape1, Tensor1}

import shapeless.Nat._1


case class StdVector(data: T1) extends Tensor1 {

  def shape: Shape[_1] = Shape1(data.size)

  def toStdFloat: Vec[Float] = data.map(_.toFloat)

  def toStdDouble: Vec[Double] = data


}
