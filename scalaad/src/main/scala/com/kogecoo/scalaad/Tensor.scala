package com.kogecoo.scalaad

import com.kogecoo.scalaad.algorithm.Value
import com.kogecoo.scalaad.impl.std.{Mat, Vec}
import shapeless.Nat
import shapeless.Nat.{_0, _1, _2}


trait Tensor[N <: Nat] {

  def shape: Shape[N]

}


trait Tensor0 extends Tensor[_0] {

  def shape: Shape[_0] = Shape0()

  def value[T](implicit v: Value[Tensor0, T]): T = v.value(this)

  def toStdFloat:  Float

  def toStdDouble: Double

}


trait Tensor1 extends Tensor[_1] {

  def shape: Shape[_1]

  def value[T](implicit v: Value[Tensor1, T]): T = v.value(this)

  def toStdFloat:  Vec[Float]

  def toStdDouble: Vec[Double]

}


trait Tensor2 extends Tensor[_2] {

  def shape: Shape[_2]

  def value[T](implicit v: Value[Tensor2, T]): T = v.value(this)

  def toStdFloat:  Mat[Float]

  def toStdDouble: Mat[Double]

}


