package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{N0, Var0}
import com.kogecoo.scalaad.op.Asin0
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdAsin0Spec extends Properties("Asin0") with Asin0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N0): T0 = math.asin(a.toT0)

  override def deriv(a: T0): T0 = 1.0 / math.sqrt(1.0 - a * a)

  override def defaultMinValue = Some(-1e10)

  override def defaultMaxValue = Some( 1e10)

  override def domain = StdValueGen(
    Some(-1.0),
    Some(1.0),
    (x: Double) => x != -1.0 && x != 1.0
  )

}

trait Asin0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  def domain: Gen[T0]

  override def op(a: N0): N0 = Asin0(a)

  override def op(argStr: String): String = s"asin($argStr)"

  override def genArgV0ForSpecBase: Gen[Var0] = genV0(domain)

  // needs to be excluded the One0 node
  override def genArgN0ForSpecBase: Gen[N0] = {
    Gen.oneOf(
      genV0(domain),
      genConst0(domain),
      genHalf0(),
      genZero0()
    )
  }

  override def genArgNV0ForSpecBase: Gen[N0] = {
    Gen.oneOf(
      genConst0(domain),
      genHalf0(),
      genZero0()
    )
  }

}


