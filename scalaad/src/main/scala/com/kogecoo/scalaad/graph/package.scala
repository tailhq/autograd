package com.kogecoo.scalaad

import Predef.{ any2stringadd => _ }


package object graph {

  type S0 = Shape0
  type S1 = Shape1
  type S2 = Shape2

  type V0 = ValueExpr[S0]
  type V1 = ValueExpr[S1]
  type V2 = ValueExpr[S2]

  type B0 = BooleanExpr[S0]
  type B1 = BooleanExpr[S1]
  type B2 = BooleanExpr[S2]

}
