package com.kogecoo.scalaad

import com.kogecoo.scalaad.algorithm.Value
import com.kogecoo.scalaad.impl.std.{StdMat, StdVec}
import shapeless.Nat
import shapeless.Nat.{_0, _1, _2}


//trait ComplexScalar { }

trait Tensor[N <: Nat] {

  def shape: Shape[N]

}


trait Tensor0 extends Tensor[_0] {

  def value[V](implicit v: Value[Tensor0, V]): V = v.value(this)

  def shape: Shape[_0] = Shape0()

  def toStdFloat:  Float

  def toStdDouble: Double

}


trait Tensor1 extends Tensor[_1] {

  val transposed: Boolean

  def value[V](implicit v: Value[Tensor1, V]): V = v.value(this)

  def shape: Shape[_1]

  def toStdFloat:  StdVec[Float]

  def toStdDouble: StdVec[Double]

}


trait Tensor2 extends Tensor[_2] {

  def value[V](implicit v: Value[Tensor2, V]): V = v.value(this)

  def shape: Shape[_2]

  def toStdFloat:  StdMat[Float]

  def toStdDouble: StdMat[Double]

}


trait BooleanTensor0 extends Tensor[_0] {

  def value[V](implicit v: Value[BooleanTensor0, V]): V = v.value(this)

  def shape: Shape[_0] = Shape0()

  def toStd: Boolean

}


trait BooleanTensor1 extends Tensor[_1] {

  def value[V](implicit v: Value[BooleanTensor1, V]): V = v.value(this)

  def shape: Shape[_1]

  def toStd: StdVec[Boolean]

}

trait BooleanTensor2 extends Tensor[_2] {

  def value[V](implicit v: Value[BooleanTensor2, V]): V = v.value(this)

  def shape: Shape[_2]

  def toStd: StdMat[Boolean]

}

