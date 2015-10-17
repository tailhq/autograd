package com.kogecoo.scalaad.test.helper.rule

import com.kogecoo.scalaad.graph.Scalar
import com.kogecoo.scalaad.rule.{ContainerValue, NonContainerValue, Value, ValueRule}


object ScalarIntValueRule {

  object Implicits {

    implicit val scalarIntValueRule = new ScalarIntValueRule

  }

}


class ScalarIntValueRule extends ValueRule[Scalar, Int]{

  override def zeroM: Int = 0
  override def zeroS(shape: Scalar[Int]): Scalar[Int] = Scalar(0)
  override def oneM: Int = 1
  override def oneS(shape: Scalar[Int]): Scalar[Int] = Scalar(1)

  override def toValue(v: Int): Value[Scalar, Int] = NonContainerValue[Scalar, Int](v)
  override def toValue(v: Scalar[Int])(implicit e: DummyImplicit): Value[Scalar, Int] = ContainerValue[Scalar, Int](v)

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

  override def transposeS(v: Scalar[Int]): Scalar[Int] = v
  override def transposeM(v: Int): Int = v

  override def whereSSS(cond: Scalar[Boolean], a: Scalar[Int], b: Scalar[Int]): Scalar[Int] = if (cond.data) a else b
  override def whereSSM(cond: Scalar[Boolean], a: Scalar[Int], b: Int):         Scalar[Int] = if (cond.data) a else Scalar(b)
  override def whereSMS(cond: Scalar[Boolean], a: Int,         b: Scalar[Int]): Scalar[Int] = if (cond.data) Scalar(a) else b
  override def whereSMM(cond: Scalar[Boolean], a: Int,         b: Int):         Scalar[Int] = if (cond.data) Scalar(a) else Scalar(b)
  override def whereMSS(cond: Boolean,         a: Scalar[Int], b: Scalar[Int]): Scalar[Int] = if (cond) a else b
  override def whereMSM(cond: Boolean,         a: Scalar[Int], b: Int):         Scalar[Int] = if (cond) a else Scalar(b)
  override def whereMMS(cond: Boolean,         a: Int,         b: Scalar[Int]): Scalar[Int] = if (cond) Scalar(a) else b
  override def whereMMM(cond: Boolean,         a: Int,         b: Int):         Int         = if (cond) a else b

}
