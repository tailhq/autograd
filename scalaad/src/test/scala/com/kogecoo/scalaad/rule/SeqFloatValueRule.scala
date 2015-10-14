package com.kogecoo.scalaad.rule


object SeqFloatValueRule {

  object Implicits {

    implicit val seqFloatValueRule = new SeqFloatValueRule

  }

}


class SeqFloatValueRule extends ValueRule[Seq, Float] {

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

  override def transposeS(v: Seq[Float]): Seq[Float] = v
  override def transposeM(v: Float): Float = v

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

