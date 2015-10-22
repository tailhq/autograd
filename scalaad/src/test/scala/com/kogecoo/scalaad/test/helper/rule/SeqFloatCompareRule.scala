package com.kogecoo.scalaad.test.helper.rule

class SeqFloatCompareRule extends CompareRule[Seq, Float] {
  override def compare(a: Seq[Float], b: Seq[Float])(implicit d: DummyImplicit): Boolean = {
    a.zip(b).forall { case (x, y) =>
      x == y || (x.equals(Float.NaN) && y.equals(Float.NaN))
    }
  }


  override def compare(a: Float, b: Float): Boolean = {
    a == b || (a.equals(Float.NaN) && b.equals(Float.NaN))
  }

}

object SeqFloatCompareRule {

  object Implicits {
    implicit val seqFloatCompareRule = new SeqFloatCompareRule

  }

}
