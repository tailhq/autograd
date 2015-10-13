package com.kogecoo.scalaad.rule

import com.kogecoo.scalaad.graph.Scalar

object ValueRuleExample {

  val valueRuleSeqFloat = new {} with ValueRule[Seq, Float] {

    override val zeroAdd: Value[Seq, Float] = toValue(0)
    override val zeroMul: Value[Seq, Float] = toValue(1)
    override val derivConst: Value[Seq, Float] = toValue(0)

    override def toValue(v: Float): Value[Seq, Float] = NonContainerValue[Seq, Float](v)

    override def toValue(v: Seq[Float])(implicit e: DummyImplicit): Value[Seq, Float] = ContainerValue[Seq, Float](v)

    override def addSS(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case(a, b) => a + b }
    override def subSS(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case(a, b) => a - b }
    override def mulSS(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case(a, b) => a * b }
    override def divSS(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case(a, b) => a / b }

    override def addSM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ + r)
    override def subSM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ - r)
    override def mulSM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ * r)
    override def divSM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ / r)

    override def addMS(l: Float, r: Seq[Float]): Seq[Float] = r.map(l + _)
    override def subMS(l: Float, r: Seq[Float]): Seq[Float] = r.map(l - _)
    override def mulMS(l: Float, r: Seq[Float]): Seq[Float] = r.map(l * _)
    override def divMS(l: Float, r: Seq[Float]): Seq[Float] = r.map(l / _)

    override def addMM(l: Float, r: Float): Float = l + r
    override def subMM(l: Float, r: Float): Float = l - r
    override def mulMM(l: Float, r: Float): Float = l * r
    override def divMM(l: Float, r: Float): Float = l / r

    override def ltSS (l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case(a, b) => a < b }
    override def lteSS(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case(a, b) => a <= b }
    override def gtSS (l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case(a, b) => a > b }
    override def gteSS(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case(a, b) => a >= b }
    override def eqSS (l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case(a, b) => a == b }

    override def ltSM (l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ < r)
    override def lteSM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ <= r)
    override def gtSM (l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ > r)
    override def gteSM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ >= r)
    override def eqSM (l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ == r)

    override def ltMS (l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l < _)
    override def lteMS(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l <= _)
    override def gtMS (l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l > _)
    override def gteMS(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l >= _)
    override def eqMS (l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l == _)

    override def ltMM (l: Float, r: Float): Boolean = l < r
    override def lteMM(l: Float, r: Float): Boolean = l <= r
    override def gtMM (l: Float, r: Float): Boolean = l > r
    override def gteMM(l: Float, r: Float): Boolean = l >= r
    override def eqMM (l: Float, r: Float): Boolean = l == r

    override def posS(v: Seq[Float]): Seq[Float] = v.map(+_)
    override def negS(v: Seq[Float]): Seq[Float] = v.map(-_)

    override def posM(v: Float): Float = +v
    override def negM(v: Float): Float = -v

    override def whereSSS(cond: Seq[Boolean], a: Seq[Float], b: Seq[Float]): Seq[Float] = {
      cond.zip(a.zip(b)).map { case (c, (x, y)) => if (c) x else y }
    }

    override def whereSSM(cond: Seq[Boolean], a: Seq[Float], b: Float): Seq[Float] = {
      cond.zip(a).map { case (c, x) => if (c) x else b }
    }

    override def whereSMS(cond: Seq[Boolean], a: Float, b: Seq[Float]): Seq[Float] = {
      cond.zip(b).map { case (c, y) => if (c) a else y }
    }

    override def whereSMM(cond: Seq[Boolean], a: Float, b: Float): Seq[Float] = {
      cond.map { case c => if (c) a else b }
    }

    override def whereMSS(cond: Boolean, a: Seq[Float], b: Seq[Float]): Seq[Float] = {
      if (cond) a else b
    }

    override def whereMSM(cond: Boolean, a: Seq[Float], b: Float): Seq[Float] = {
      if (cond) a else a.map(_ => b)
    }

    override def whereMMS(cond: Boolean, a: Float, b: Seq[Float]): Seq[Float] = {
      if (cond) b.map(_ => a) else b
    }

    override def whereMMM(cond: Boolean, a: Float, b: Float): Float = {
      if (cond) a else b
    }

  }

  val valueRuleScalarInt = new {} with ValueRule[Scalar, Int] {

    override val zeroAdd: Value[Scalar, Int] = toValue(0)
    override val zeroMul: Value[Scalar, Int] = toValue(1)
    override val derivConst: Value[Scalar, Int] = toValue(0)

    override def toValue(v: Int): Value[Scalar, Int] = NonContainerValue[Scalar, Int](v)

    override def toValue(v: Scalar[Int])(implicit e: DummyImplicit): Value[Scalar, Int] = NonContainerValue[Scalar, Int](v.data)

    override def addSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Int] = Scalar(l.data + r.data)
    override def subSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Int] = Scalar(l.data - r.data)
    override def mulSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Int] = Scalar(l.data * r.data)
    override def divSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Int] = Scalar(l.data / r.data)

    override def addSM(l: Scalar[Int], r: Int): Scalar[Int] = Scalar(l.data + r)
    override def subSM(l: Scalar[Int], r: Int): Scalar[Int] = Scalar(l.data - r)
    override def mulSM(l: Scalar[Int], r: Int): Scalar[Int] = Scalar(l.data * r)
    override def divSM(l: Scalar[Int], r: Int): Scalar[Int] = Scalar(l.data / r)

    override def addMS(l: Int, r: Scalar[Int]): Scalar[Int] = Scalar(l + r.data)
    override def subMS(l: Int, r: Scalar[Int]): Scalar[Int] = Scalar(l - r.data)
    override def mulMS(l: Int, r: Scalar[Int]): Scalar[Int] = Scalar(l * r.data)
    override def divMS(l: Int, r: Scalar[Int]): Scalar[Int] = Scalar(l / r.data)

    override def addMM(l: Int, r: Int): Int = l + r
    override def subMM(l: Int, r: Int): Int = l - r
    override def mulMM(l: Int, r: Int): Int = l * r
    override def divMM(l: Int, r: Int): Int = l / r

    override def ltSS (l: Scalar[Int], r: Scalar[Int]): Scalar[Boolean] = Scalar(l.data < r.data)
    override def lteSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Boolean] = Scalar(l.data <= r.data)
    override def gtSS (l: Scalar[Int], r: Scalar[Int]): Scalar[Boolean] = Scalar(l.data > r.data)
    override def gteSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Boolean] = Scalar(l.data >= r.data)
    override def eqSS (l: Scalar[Int], r: Scalar[Int]): Scalar[Boolean] = Scalar(l.data == r.data)

    override def ltSM (l: Scalar[Int], r: Int): Scalar[Boolean] = Scalar(l.data < r)
    override def lteSM(l: Scalar[Int], r: Int): Scalar[Boolean] = Scalar(l.data <= r)
    override def gtSM (l: Scalar[Int], r: Int): Scalar[Boolean] = Scalar(l.data > r)
    override def gteSM(l: Scalar[Int], r: Int): Scalar[Boolean] = Scalar(l.data >= r)
    override def eqSM (l: Scalar[Int], r: Int): Scalar[Boolean] = Scalar(l.data == r)

    override def ltMS (l: Int, r: Scalar[Int]): Scalar[Boolean] = Scalar(l < r.data)
    override def lteMS(l: Int, r: Scalar[Int]): Scalar[Boolean] = Scalar(l < r.data)
    override def gtMS (l: Int, r: Scalar[Int]): Scalar[Boolean] = Scalar(l < r.data)
    override def gteMS(l: Int, r: Scalar[Int]): Scalar[Boolean] = Scalar(l < r.data)
    override def eqMS (l: Int, r: Scalar[Int]): Scalar[Boolean] = Scalar(l < r.data)

    override def ltMM (l: Int, r: Int): Boolean = l < r
    override def lteMM(l: Int, r: Int): Boolean = l <= r
    override def gtMM (l: Int, r: Int): Boolean = l > r
    override def gteMM(l: Int, r: Int): Boolean = l >= r
    override def eqMM (l: Int, r: Int): Boolean = l == r

    override def posS(v: Scalar[Int]): Scalar[Int] = Scalar(+v.data)
    override def negS(v: Scalar[Int]): Scalar[Int] = Scalar(-v.data)

    override def posM(v: Int): Int = +v
    override def negM(v: Int): Int = -v

    override def whereSSS(cond: Scalar[Boolean], a: Scalar[Int], b: Scalar[Int]): Scalar[Int] = if (cond.data) a else b
    override def whereSSM(cond: Scalar[Boolean], a: Scalar[Int], b: Int):         Scalar[Int] = if (cond.data) a else Scalar(b)
    override def whereSMS(cond: Scalar[Boolean], a: Int,         b: Scalar[Int]): Scalar[Int] = if (cond.data) Scalar(a) else b
    override def whereSMM(cond: Scalar[Boolean], a: Int,         b: Int):         Scalar[Int] = if (cond.data) Scalar(a) else Scalar(b)
    override def whereMSS(cond: Boolean,         a: Scalar[Int], b: Scalar[Int]): Scalar[Int] = if (cond) a else b
    override def whereMSM(cond: Boolean,         a: Scalar[Int], b: Int):         Scalar[Int] = if (cond) a else Scalar(b)
    override def whereMMS(cond: Boolean,         a: Int,         b: Scalar[Int]): Scalar[Int] = if (cond) Scalar(a) else b
    override def whereMMM(cond: Boolean,         a: Int,         b: Int):         Int         = if (cond) a else b

  }


}
