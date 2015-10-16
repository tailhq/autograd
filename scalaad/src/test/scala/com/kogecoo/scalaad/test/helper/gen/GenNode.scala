package com.kogecoo.scalaad.test.helper.gen

import com.kogecoo.scalaad.graph.{Node, ContainerConst, Var, ScalarConst}
import org.scalacheck.Gen

import scala.language.higherKinds


abstract class GenNode[U[_], T] {

  def genNode: Gen[Node[U, T]]

  def genVar: Gen[Var[U, T]]

  def genScalarConst: Gen[ScalarConst[U, T]]

  def genContainerConst: Gen[ContainerConst[U, T]]

}
