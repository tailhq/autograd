package scalaad.impl.breeze.op

import breeze.linalg
import breeze.linalg.operators

import scalaad.impl.breeze.{B0, B1, B2, T0, T1, T2}


object Op {

  object add extends BroadcastOp {

    def exchangable: B0 = true

    def baseOpImpl000(a: T0, b: T0): T0 = a + b

    def baseOpImpl101(a: T1, b: T0): T1 = a :+ b

    def baseOpImpl111(a: T1, b: T1): T1 = a :+ b

    def baseOpImpl202(a: T2, b: T0): T2 = a :+ b

    def baseOpImpl222(a: T2, b: T2): T2 = a :+ b

  }


  object sub extends BroadcastOp {

    def exchangable: B0 = false

    def baseOpImpl000(a: T0, b: T0): T0 = a - b

    def baseOpImpl101(a: T1, b: T0): T1 = a :- b

    def baseOpImpl111(a: T1, b: T1): T1 = a :- b

    def baseOpImpl202(a: T2, b: T0): T2 = a :- b

    def baseOpImpl222(a: T2, b: T2): T2 = a :- b

  }


  object mul extends BroadcastOp {

    def exchangable: B0 = true

    def baseOpImpl000(a: T0, b: T0): T0 = a * b

    def baseOpImpl101(a: T1, b: T0): T1 = a :* b

    def baseOpImpl111(a: T1, b: T1): T1 = a :* b

    def baseOpImpl202(a: T2, b: T0): T2 = a :* b

    def baseOpImpl222(a: T2, b: T2): T2 = a :* b

  }


  object div extends BroadcastOp {

    def exchangable: B0 = false

    def baseOpImpl000(a: T0, b: T0): T0 = a / b

    def baseOpImpl101(a: T1, b: T0): T1 = a :/ b

    def baseOpImpl111(a: T1, b: T1): T1 = a :/ b

    def baseOpImpl202(a: T2, b: T0): T2 = a :/ b

    def baseOpImpl222(a: T2, b: T2): T2 = a :/ b

  }


  object pow extends BroadcastOp {

    def exchangable: B0 = false

    def baseOpImpl000(a: T0, b: T0): T0 = math.pow(a, b)

    def baseOpImpl101(a: T1, b: T0): T1 = operators.OpPow(a, b)

    def baseOpImpl111(a: T1, b: T1): T1 = operators.OpPow(a, b)

    def baseOpImpl202(a: T2, b: T0): T2 = operators.OpPow(a, b)

    def baseOpImpl222(a: T2, b: T2): T2 = operators.OpPow(a, b)

  }


  object max extends BroadcastOp {

    def exchangable: B0 = true

    def baseOpImpl000(a: T0, b: T0): T0 = linalg.max(a, b)

    def baseOpImpl101(a: T1, b: T0): T1 = linalg.max(a, b)

    def baseOpImpl111(a: T1, b: T1): T1 = linalg.max(a, b)

    def baseOpImpl202(a: T2, b: T0): T2 = linalg.max(a, b)

    def baseOpImpl222(a: T2, b: T2): T2 = linalg.max(a, b)

  }


  object min extends BroadcastOp {

    def exchangable: B0 = true

    def baseOpImpl000(a: T0, b: T0): T0 = linalg.min(a, b)

    def baseOpImpl101(a: T1, b: T0): T1 = linalg.min(a, b)

    def baseOpImpl111(a: T1, b: T1): T1 = linalg.min(a, b)

    def baseOpImpl202(a: T2, b: T0): T2 = linalg.min(a, b)

    def baseOpImpl222(a: T2, b: T2): T2 = linalg.min(a, b)

  }

  object eq extends BroadcastComparisonOp {

    def exchangable: B0 = true

    def baseOpImpl000(a: T0, b: T0): B0 = a == b

    def baseOpImpl101(a: T1, b: T0): B1 = a :== b

    def baseOpImpl111(a: T1, b: T1): B1 = a :== b

    def baseOpImpl202(a: T2, b: T0): B2 = a :== b

    def baseOpImpl222(a: T2, b: T2): B2 = a :== b

  }


  object neq extends BroadcastComparisonOp {

    def exchangable: B0 = true

    def baseOpImpl000(a: T0, b: T0): B0 = a != b

    def baseOpImpl101(a: T1, b: T0): B1 = a :!= b

    def baseOpImpl111(a: T1, b: T1): B1 = a :!= b

    def baseOpImpl202(a: T2, b: T0): B2 = a :!= b

    def baseOpImpl222(a: T2, b: T2): B2 = a :!= b

  }


  object lt extends BroadcastComparisonOp {

    def exchangable: B0 = false

    def baseOpImpl000(a: T0, b: T0): B0 = a < b

    def baseOpImpl101(a: T1, b: T0): B1 = a :< b

    def baseOpImpl111(a: T1, b: T1): B1 = a :< b

    def baseOpImpl202(a: T2, b: T0): B2 = a :< b

    def baseOpImpl222(a: T2, b: T2): B2 = a :< b

  }


  object lte extends BroadcastComparisonOp {

    def exchangable: B0 = false

    def baseOpImpl000(a: T0, b: T0): B0 = a <= b

    def baseOpImpl101(a: T1, b: T0): B1 = a :<= b

    def baseOpImpl111(a: T1, b: T1): B1 = a :<= b

    def baseOpImpl202(a: T2, b: T0): B2 = a :<= b

    def baseOpImpl222(a: T2, b: T2): B2 = a :<= b

  }


  object gt extends BroadcastComparisonOp {

    def exchangable: B0 = false

    def baseOpImpl000(a: T0, b: T0): B0 = a > b

    def baseOpImpl101(a: T1, b: T0): B1 = a :> b

    def baseOpImpl111(a: T1, b: T1): B1 = a :> b

    def baseOpImpl202(a: T2, b: T0): B2 = a :> b

    def baseOpImpl222(a: T2, b: T2): B2 = a :> b

  }


  object gte extends BroadcastComparisonOp {

    def exchangable: B0 = false

    def baseOpImpl000(a: T0, b: T0): B0 = a >= b

    def baseOpImpl101(a: T1, b: T0): B1 = a :>= b

    def baseOpImpl111(a: T1, b: T1): B1 = a :>= b

    def baseOpImpl202(a: T2, b: T0): B2 = a :>= b

    def baseOpImpl222(a: T2, b: T2): B2 = a :>= b

  }


  object and extends BroadcastBoolOp {

    def exchangable: B0 = true

    def baseOpImpl000(a: B0, b: B0): B0 = a && b

    def baseOpImpl101(a: B1, b: B0): B1 = a :& BroadcastHelper.vecFillLike(b, a)

    def baseOpImpl111(a: B1, b: B1): B1 = a :& b

    def baseOpImpl202(a: B2, b: B0): B2 = a :& BroadcastHelper.matFillLike(b, a)

    def baseOpImpl222(a: B2, b: B2): B2 = a :& b

  }


  object or extends BroadcastBoolOp {

    def exchangable: B0 = true

    def baseOpImpl000(a: B0, b: B0): B0 = a || b

    def baseOpImpl101(a: B1, b: B0): B1 = a :| BroadcastHelper.vecFillLike(b, a)

    def baseOpImpl111(a: B1, b: B1): B1 = a :| b

    def baseOpImpl202(a: B2, b: B0): B2 = a :| BroadcastHelper.matFillLike(b, a)

    def baseOpImpl222(a: B2, b: B2): B2 = a :| b

  }


}

