package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.graph.{ContainerConst, ScalarConst, Var}

import scala.language.higherKinds


class VarSample[U[_], T](val node: Var[U, T], val src: U[T]) {
  override def toString = s"VarSample(${node}, ${src})"
}

class ScalarConstSample[U[_], T](val node: ScalarConst[U, T], val src: T) {
  override def toString = s"ScalarConstSample(${node}), ${src})"
}

class ContainerConstSample[U[_], T](val node: ContainerConst[U, T], val src: U[T]) {
  override def toString = s"ContainerConstSample(${node}, ${src})"
}
