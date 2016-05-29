package com.kogecoo.scalaad


package object op {

  type Op0 = UnaryOp[S0, S0]

  type Op1 = UnaryOp[S1, S1]

  type Op2 = UnaryOp[S2, S2]


  type Op00 = BinaryOp[S0, S0, S0]

  type Op11 = BinaryOp[S1, S1, S1]

  type Op22 = BinaryOp[S2, S2, S2]


  type Op0B = UnaryBooleanOp[S0, S0]

  type Op1B = UnaryBooleanOp[S1, S1]

  type Op2B = UnaryBooleanOp[S2, S2]


  type Op00B = BinaryBooleanOp[S0, S0, S0]

  type Op11B = BinaryBooleanOp[S1, S1, S1]

  type Op22B = BinaryBooleanOp[S2, S2, S2]


  type Op0C = UnaryComparisonOp[S0, S0]

  type Op1C = UnaryComparisonOp[S1, S1]

  type Op2C = UnaryComparisonOp[S2, S2]


  type Op00C = BinaryComparisonOp[S0, S0, S0]

  type Op11C = BinaryComparisonOp[S1, S1, S1]

  type Op22C = BinaryComparisonOp[S2, S2, S2]


}
