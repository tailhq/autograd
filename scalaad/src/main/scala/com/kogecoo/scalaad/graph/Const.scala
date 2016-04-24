package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad._


trait ConstNode0 extends V0 { override val shape: S0 = Shape0() }

trait ConstNode1 extends V1

trait ConstNode2 extends V2


case class Const0(data: Tensor0) extends ConstNode0

case class Const1(data: Tensor1, shape: S1) extends ConstNode1

case class Const2(data: Tensor2, shape: S2) extends ConstNode2


case class One0() extends ConstNode0

case class One1(shape: S1) extends ConstNode1

case class One2(shape: S2) extends ConstNode2

case class Eye2(shape: S2) extends ConstNode2


case class Zero0() extends ConstNode0

case class Zero1(shape: S1) extends ConstNode1

case class Zero2(shape: S2) extends ConstNode2


case class Half0() extends ConstNode0

case class Half1(shape: S1) extends ConstNode1

case class Half2(shape: S2) extends ConstNode2


object One1 { def apply(n: V1): One1 = One1(n.shape) }

object One2 { def apply(n: V2): One2 = One2(n.shape) }


object Zero1 { def apply(n: V1): Zero1 = Zero1(n.shape) }

object Zero2 { def apply(n: V2): Zero2 = Zero2(n.shape) }


object Half1 { def apply(n: V1): Half1 = Half1(n.shape) }

object Half2 { def apply(n: V2): Half2 = Half2(n.shape) }

