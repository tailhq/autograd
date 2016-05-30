package com.kogecoo.scalaad.analyze

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.ValueExpr


class Analyzing {

  private[this] val eqnProjBuilder = Map.newBuilder[Param[_ <: Shape], Equation[_ <: Shape]]

  private[this] val paramProjBuilder = Map.newBuilder[Param[_ <: Shape], ValueExpr[_ <: Shape]]

  def addEqn[S <: Shape](param: Param[S], eqn: Equation[S]): Param[S]= {
    eqnProjBuilder += ((param, eqn))
    param
  }

  def addParam[S <: Shape](param: Param[S], v: ValueExpr[S]): Param[S] = {
    paramProjBuilder += ((param, v))
    param
  }

  def result(): Analyzed = new Analyzed(eqnProjBuilder.result(), paramProjBuilder.result())

}

class Analyzed(
  eqns: Map[Param[_ <: Shape], Equation[_ <: Shape]],
  params: Map[Param[_ <: Shape], ValueExpr[_ <: Shape]]
)
