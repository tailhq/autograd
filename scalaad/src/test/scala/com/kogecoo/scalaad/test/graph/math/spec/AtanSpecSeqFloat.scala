package com.kogecoo.scalaad.test.graph.math.spec

import com.kogecoo.scalaad.graph.{atan, cos, Node}
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpSpec, UnaryOpExpectedBehaviorDef}
import org.scalacheck.Properties

import scala.language.higherKinds


object ATanSpecSeqFloat extends Properties("ATan - Seq[Float]") {

  implicit val compareRule = new SeqFloatSoftCompareRule

  val nodeGen  = new SeqFloatNodeGenWithValueRange(-1e15f, 1e15f)
  val valueGen = new SeqFloatValueGenWithValueRange(-1e15f, 1e15f)
  val expects  = new TanSeqFloatExpectedBehavior

  val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

  property("atan(a) apply") = seqFloatSpecGen.applyScalar()
  property("atan(a) apply") = seqFloatSpecGen.applyContainer()
  property("atan(a) apply") = seqFloatSpecGen.applyVar()

  property("atan(a) (scalar) w.r.t. a")    = seqFloatSpecGen.derivScalarWrtSelf()
  property("atan(a) (scalar) w.r.t. b")    = seqFloatSpecGen.derivScalarWrtUnknown()
  property("atan(a) (container) w.r.t. a") = seqFloatSpecGen.derivContainerWrtSelf()
  property("atan(a) (container) w.r.t. b") = seqFloatSpecGen.derivContainerWrtUnknown()
  property("atan(a) (var) w.r.t. a")       = seqFloatSpecGen.derivContainerWrtSelf()
  property("atan(a) (var) w.r.t. b")       = seqFloatSpecGen.derivContainerWrtUnknown()

  property("atan(a) (scalar) propagete with value")        = seqFloatSpecGen.propagateScalarWithNCValue()
  property("atan(a) (scalar) propagate with container")    = seqFloatSpecGen.propagateScalarWithCValue()
  property("atan(a) (container) propagete with value")     = seqFloatSpecGen.propagateContainerWithNCValue()
  property("atan(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
  property("atan(a) (container) propagete with value")     = seqFloatSpecGen.propagateVarWithNCValue()
  property("atan(a) (container) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

  property("atan(a) grad")              = seqFloatSpecGen.gradScalar()
  property("atan(a) grad")              = seqFloatSpecGen.gradContainer()
  property("atan(a) grad")              = seqFloatSpecGen.gradVar()

}


class ATanSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

  override val zero: Float = 0f

  override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

  override def op(node: Node[Seq, Float]): Node[Seq, Float] = atan(node)

  override def applyScalar(a: Float): Float              = scala.math.atan(a).toFloat
  override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.atan(x.toDouble).toFloat }
  override def applyVar(a: Seq[Float]): Seq[Float]       = a.map { x => scala.math.atan(x.toDouble).toFloat }

  override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => 1f / (1f + x * x) }

  override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => b / (1f + x * x) }

  override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) =>
    y / (1f + x * x)
  }

  override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => 1f / (1f + x * x) }

}
