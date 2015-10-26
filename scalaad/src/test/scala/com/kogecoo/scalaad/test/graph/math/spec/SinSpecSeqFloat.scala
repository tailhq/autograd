package com.kogecoo.scalaad.test.graph.math.spec

import com.kogecoo.scalaad.graph.{sin, Node}
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpSpec, UnaryOpExpectedBehaviorDef}
import org.scalacheck.Properties

import scala.language.higherKinds


object SinSpecSeqFloat extends Properties("Sin - Seq[Float]") {

  implicit val compareRule = new SeqFloatSoftCompareRule

  val nodeGen  = new SeqFloatNodeGen
  val valueGen = new SeqFloatValueGen
  val expects  = new SinSeqFloatExpectedBehavior

  val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

  property("+a apply") = seqFloatSpecGen.applyScalar()
  property("+a apply") = seqFloatSpecGen.applyContainer()
  property("+a apply") = seqFloatSpecGen.applyVar()

  property("+a (scalar) w.r.t. a")    = seqFloatSpecGen.derivScalarWrtSelf()
  property("+a (scalar) w.r.t. b")    = seqFloatSpecGen.derivScalarWrtUnknown()
  property("+a (container) w.r.t. a") = seqFloatSpecGen.derivContainerWrtSelf()
  property("+a (container) w.r.t. b") = seqFloatSpecGen.derivContainerWrtUnknown()
  property("+a (var) w.r.t. a")       = seqFloatSpecGen.derivContainerWrtSelf()
  property("+a (var) w.r.t. b")       = seqFloatSpecGen.derivContainerWrtUnknown()

  property("+a (scalar) propagete with value")        = seqFloatSpecGen.propagateScalarWithNCValue()
  property("+a (scalar) propagate with container")    = seqFloatSpecGen.propagateScalarWithCValue()
  property("+a (container) propagete with value")     = seqFloatSpecGen.propagateContainerWithNCValue()
  property("+a (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
  property("+a (container) propagete with value")     = seqFloatSpecGen.propagateVarWithNCValue()
  property("+a (container) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

  property("+a grad")              = seqFloatSpecGen.gradScalar()
  property("+a grad")              = seqFloatSpecGen.gradContainer()
  property("+a grad")              = seqFloatSpecGen.gradVar()

}


class SinSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

  override val zero: Float = 0f

  override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

  override def op(node: Node[Seq, Float]): Node[Seq, Float] = sin(node)

  override def applyScalar(a: Float): Float              = scala.math.sin(a).toFloat
  override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.sin(x.toDouble).toFloat }
  override def applyVar(a: Seq[Float]): Seq[Float]       = a.map { x => scala.math.sin(x.toDouble).toFloat }

  override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.cos(x.toDouble).toFloat }

  override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => b * scala.math.cos(x.toDouble).toFloat }
  override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => y * scala.math.cos(x.toDouble).toFloat }

  override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.cos(x.toDouble).toFloat }

}
