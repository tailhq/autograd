package com.kogecoo.scalaad

import com.kogecoo.scalaad.algorithm.Value
import com.kogecoo.scalaad.impl.std.{Mat, Vec}
import shapeless.Nat
import shapeless.Nat.{_0, _1, _2}

import scala.language.higherKinds


trait BooleanTensor[N <: Nat] {

  def shape: Shape[N]

}


trait BooleanTensor0 extends Tensor[_0] {

  def value[V](implicit v: Value[BooleanTensor0, V]): V = v.value(this)

  def shape: Shape[_0] = Shape0()

  def toStd: Boolean

}


trait BooleanTensor1 extends Tensor[_1] {

  def value[V](implicit v: Value[BooleanTensor1, V]): V = v.value(this)

  def shape: Shape[_1]

  def toStd: Vec[Boolean]

}


trait BooleanTensor2 extends Tensor[_2] {

  def value[V](implicit v: Value[BooleanTensor2, V]): V = v.value(this)

  def shape: Shape[_2]

  def toStd: Mat[Boolean]

}

