package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{V0, Var0}
import com.kogecoo.scalaad.op.Ln0
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdLn0Spec extends Properties("Ln0") with Ln0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: V0): T0 = math.log(a.toT0)

  override def deriv(a: T0): T0 =  1.0 / a

  override def defaultMinValue = Some(0.0)

  override def defaultMaxValue = Some(100.0)

  override def defaultValueConstraint = (x: Double) => x != -0.0 && x != 0.0

}

trait Ln0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: V0): V0 = Ln0(a)

  override def op(argStr: String): String = s"ln($argStr)"


  override def genArgN0ForSpecBase: Gen[V0] = genNonzeroN0()

  override def genArgNV0ForSpecBase: Gen[V0] = genNonzeroNV0()

}

