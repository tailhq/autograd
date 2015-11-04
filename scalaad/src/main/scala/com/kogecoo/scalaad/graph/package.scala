package com.kogecoo.scalaad

import Predef.{ any2stringadd => _ }


package object graph {

  type S0 = Shape0
  type S1 = Shape1
  type S2 = Shape2

  type N0 = Node[S0]
  type N1 = Node[S1]
  type N2 = Node[S2]

  type Op0 = ShapeEquivUnaryOp[S0]
  type Op1 = ShapeEquivUnaryOp[S1]
  type Op2 = ShapeEquivUnaryOp[S2]

  type Op00 = ShapeEquivBinaryOp[S0]
  type Op11 = ShapeEquivBinaryOp[S1]
  type Op22 = ShapeEquivBinaryOp[S2]

  type Op01 = RightShapedBinaryOp[S0, S1]
  type Op10 =  LeftShapedBinaryOp[S1, S0]
  type Op02 = RightShapedBinaryOp[S0, S2]
  type Op20 =  LeftShapedBinaryOp[S2, S0]
  type Op12 = RightShapedBinaryOp[S1, S2]
  type Op21 =  LeftShapedBinaryOp[S2, S1]

  type B0 = BoolNode[S0]
  type B1 = BoolNode[S1]
  type B2 = BoolNode[S2]

  type Cond0 = ShapeEquivUnaryCond[S0]
  type Cond1 = ShapeEquivUnaryCond[S1]
  type Cond2 = ShapeEquivUnaryCond[S2]

  type Cond00 = ShapeEquivBinaryCond[S0]
  type Cond11 = ShapeEquivBinaryCond[S1]
  type Cond22 = ShapeEquivBinaryCond[S2]

  type Cond01 = RightShapedBinaryCond[S0, S1]
  type Cond10 =  LeftShapedBinaryCond[S1, S0]
  type Cond02 = RightShapedBinaryCond[S0, S2]
  type Cond20 =  LeftShapedBinaryCond[S2, S0]
  type Cond12 = RightShapedBinaryCond[S1, S2]
  type Cond21 =  LeftShapedBinaryCond[S2, S1]

}
