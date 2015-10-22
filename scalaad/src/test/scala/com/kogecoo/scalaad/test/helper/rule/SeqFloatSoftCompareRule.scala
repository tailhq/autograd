package com.kogecoo.scalaad.test.helper.rule

class SeqFloatSoftCompareRule extends CompareRule[Seq, Float] {

  lazy val eps = 1e-4

  override def compare(a: Seq[Float], b: Seq[Float])(implicit d: DummyImplicit): Boolean = {
    a.zip(b).map({ case (x, y) =>
      scala.math.abs(x - y) <= eps || (x.equals(Float.NaN) && x.equals(Float.NaN))
    }).forall(b => b)
  }


  override def compare(a: Float, b: Float): Boolean = {
    scala.math.abs(a - b) <= eps || (a.equals(Float.NaN) && b.equals(Float.NaN))
  }

}

object SeqFloatSoftCompareRule {

  object Implicits {
    implicit val seqFloatSoftCompareRule = new SeqFloatSoftCompareRule

  }

}
