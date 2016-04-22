package com.kogecoo.scalaad

import Predef.{ any2stringadd => _ }


package object graph {

  type S0 = Shape0
  type S1 = Shape1
  type S2 = Shape2

  type N0 = ValueExpr[S0]
  type N1 = ValueExpr[S1]
  type N2 = ValueExpr[S2]

  type B0 = BooleanExpr[S0]
  type B1 = BooleanExpr[S1]
  type B2 = BooleanExpr[S2]

}
