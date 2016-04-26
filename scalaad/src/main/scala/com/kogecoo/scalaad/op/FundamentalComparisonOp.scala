package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply00C, B0, V0}


case object Eq00 extends ComparisonOp00 { def apply(l: V0, r: V0): B0 = Apply00C(l, r, Eq00)}

case object Neq00 extends ComparisonOp00 { def apply(l: V0, r: V0): B0 = Apply00C(l, r, Neq00)}

case object Lt00 extends ComparisonOp00 { def apply(l: V0, r: V0): B0 = Apply00C(l, r, Lt00)}

case object Lte00 extends ComparisonOp00 { def apply(l: V0, r: V0): B0 = Apply00C(l, r, Lte00)}

case object Gt00 extends ComparisonOp00 { def apply(l: V0, r: V0): B0 = Apply00C(l, r, Gt00)}

case object Gte00 extends ComparisonOp00 { def apply(l: V0, r: V0): B0 = Apply00C(l, r, Gte00)}
