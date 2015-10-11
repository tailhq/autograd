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

    override def posS(v: Seq[Float]): Seq[Float] = v.map(+_)
    override def negS(v: Seq[Float]): Seq[Float] = v.map(-_)

    override def posM(v: Float): Float = +v
    override def negM(v: Float): Float = -v

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

    override def posS(v: Scalar[Int]): Scalar[Int] = Scalar(+v.data)
    override def negS(v: Scalar[Int]): Scalar[Int] = Scalar(-v.data)

    override def posM(v: Int): Int = +v
    override def negM(v: Int): Int = -v

  }


}
