package com.kogecoo.scalaad

import com.kogecoo.scalaad.algorithm.Value
import com.kogecoo.scalaad.graph.{S0, S1, S2}
import com.kogecoo.scalaad.impl.std.{StdMat, StdVec}
import shapeless.Nat


//trait ComplexScalar { }

trait Tensor[N <: Nat]


trait Tensor0 extends Tensor[Nat._0] {
  def value[V](implicit v: Value[Tensor0, V]): V = v.value(this)
  def shape: S0 = Shape0()
  def toStdFloat:  Float
  def toStdDouble: Double
}

trait Tensor1 extends Tensor[Nat._1] {
  val transposed: Boolean
  def value[V](implicit v: Value[Tensor1, V]): V = v.value(this)
  def shape: S1
  def toStdFloat:  StdVec[Float]
  def toStdDouble: StdVec[Double]
}

trait Tensor2 extends Tensor[Nat._2] {
  def value[V](implicit v: Value[Tensor2, V]): V = v.value(this)
  def shape: S2
  def toStdFloat:  StdMat[Float]
  def toStdDouble: StdMat[Double]
}


trait BoolTensor0 extends Tensor[Nat._0] {
  def value[V](implicit v: Value[BoolTensor0, V]): V = v.value(this)
  def shape: S0 = Shape0()
  def toStd: Boolean
}

trait BoolTensor1 extends Tensor[Nat._1] {
  def value[V](implicit v: Value[BoolTensor1, V]): V = v.value(this)
  def shape: S1
  def toStd: StdVec[Boolean]
}

trait BoolTensor2 extends Tensor[Nat._2] {
  def value[V](implicit v: Value[BoolTensor2, V]): V = v.value(this)
  def shape: S2
  def toStd: StdMat[Boolean]
}


