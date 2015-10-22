package com.kogecoo.scalaad.test.helper.rule


class SeqFloatCompareRule extends CompareRule[Seq, Float] {

  lazy val defaultEPS = 1e-4f

  override def eq(a: Seq[Float], b: Seq[Float])(implicit d: DummyImplicit): Boolean = {
    a.zip(b).forall { case (x, y) =>
      x == y || (x.equals(Float.NaN) && y.equals(Float.NaN))
    }
  }

  override def eq(a: Float, b: Float): Boolean = {
    a == b || (a.equals(Float.NaN) && b.equals(Float.NaN))
  }

  override def closeTo(a: Seq[Float], b: Seq[Float], eps: Float)(implicit d: DummyImplicit): Boolean = {
    a.zip(b).map({ case (x, y) =>
      scala.math.abs(x - y) <= eps || (x.equals(Float.NaN) && x.equals(Float.NaN))
    }).forall(b => b)
  }


  override def closeTo(a: Float, b: Float, eps: Float): Boolean = {
    scala.math.abs(a - b) <= eps || (a.equals(Float.NaN) && b.equals(Float.NaN))
  }

}

object SeqFloatCompareRule {

  object Implicits {
    implicit val seqFloatCompareRule = new SeqFloatCompareRule

  }

}
