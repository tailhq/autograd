package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.V0
import com.kogecoo.scalaad.op.Tanh0
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdTanh0Spec extends Properties("Tanh0") with Tanh0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V0): T0 = math.tanh(a.toT0)

  override def deriv(a: T0): T0 =  1 - math.tanh(a) * math.tanh(a)

}


trait Tanh0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: V0): V0 = Tanh0(a)

  override def op(argStr: String): String = s"tanh($argStr)"

}


