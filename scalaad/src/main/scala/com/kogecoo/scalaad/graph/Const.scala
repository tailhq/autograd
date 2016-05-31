package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad._
import com.kogecoo.scalaad.analyze.{Analyzing, Param}


trait ConstBase[S <: Shape] extends ValueExpr[S] {

  def analyze(a: Analyzing): Param[S] = a.addParam(this)

}

trait ConstBase0 extends ConstBase[S0] { override def shape: S0 = Shape0() }


case class Const0(data: Tensor0) extends ConstBase0

case class Const1(data: Tensor1, shape: S1) extends ConstBase[S1]

case class Const2(data: Tensor2, shape: S2) extends ConstBase[S2]


case class One0() extends ConstBase0

case class One1(shape: S1) extends ConstBase[S1]

case class One2(shape: S2) extends ConstBase[S2]

case class Eye2(shape: S2) extends ConstBase[S2]


case class Zero0() extends ConstBase0

case class Zero1(shape: S1) extends ConstBase[S1]

case class Zero2(shape: S2) extends ConstBase[S2]


case class Half0() extends ConstBase0

case class Half1(shape: S1) extends ConstBase[S1]

case class Half2(shape: S2) extends ConstBase[S2]
