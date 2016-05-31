package com.kogecoo.scalaad.analyze

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.ValueExpr

import scala.collection.GenMap


class Analyzing {

  private[this] val eqnProjBuilder = GenMap.newBuilder[Param[_ <: Shape], Equation[_ <: Shape]]

  private[this] val paramProjBuilder = GenMap.newBuilder[Param[_ <: Shape], ValueExpr[_ <: Shape]]

  def addEqn[S <: Shape](eqn: Equation[S]): Param[S]= {
    eqnProjBuilder += ((eqn.left, eqn))
    eqn.left
  }

  def addParam[S <: Shape](v: ValueExpr[S]): Param[S] = {
    val p = Param[S]()
    paramProjBuilder += ((p, v))
    p
  }

  def result(): Analyzed = new Analyzed(eqnProjBuilder.result(), paramProjBuilder.result())

}

