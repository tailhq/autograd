package io.github.tailabs.autograd.test.graph.op.spec

import io.github.tailabs.autograd.graph.{Node, Pos}
import io.github.tailabs.autograd.rule._
import io.github.tailabs.autograd.test.helper.gen._
import io.github.tailabs.autograd.test.helper.rule.SeqFloatSoftCompareRule
import io.github.tailabs.autograd.test.helper.rule.SeqFloatValueRule.Implicits._
import io.github.tailabs.autograd.test.helper.specgen.{UnaryOpSpec, UnaryOpExpectedBehaviorDef}
import org.scalacheck.Properties

import scala.language.higherKinds


object PosSpecSeqFloat extends Properties("Pos - Seq[Float]") {

  implicit val compareRule = new SeqFloatSoftCompareRule

  val nodeGen  = new SeqFloatNodeGen
  val valueGen = new SeqFloatValueGen
  val expects  = new PosSeqFloatExpectedBehavior

  val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

  property("+a apply") = seqFloatSpecGen.applyScalar()
  property("+a apply") = seqFloatSpecGen.applyContainer()
  property("+a apply") = seqFloatSpecGen.applyVar()

  property("+a (scalar) w.r.t. unknown var")    = seqFloatSpecGen.derivScalarWrtUnknownVar()
  property("+a (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
  property("+a (var) w.r.t. a")                 = seqFloatSpecGen.derivVarWrtSelf()
  property("+a (var) w.r.t. unknonw var")       = seqFloatSpecGen.derivVarWrtUnknownVar()

  property("+a (scalar) propagete with value")        = seqFloatSpecGen.propagateScalarWithNCValue()
  property("+a (scalar) propagate with container")    = seqFloatSpecGen.propagateScalarWithCValue()
  property("+a (container) propagete with value")     = seqFloatSpecGen.propagateContainerWithNCValue()
  property("+a (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
  property("+a (var) propagete with value")           = seqFloatSpecGen.propagateVarWithNCValue()
  property("+a (var) propagate with container")       = seqFloatSpecGen.propagateVarWithCValue()

  property("+a (scalar) grad")    = seqFloatSpecGen.gradScalar()
  property("+a (container) grad") = seqFloatSpecGen.gradContainer()
  property("+a (var) grad")       = seqFloatSpecGen.gradVar()

}


class PosSeqFloatExpectedBehavior(implicit vr: ValueRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

  override val zero: Float = 0f

  override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

  override def op(node: Node[Seq, Float]): Node[Seq, Float] = Pos(node)

  override def applyScalar(a: Float): Float              = +a
  override def applyContainer(a: Seq[Float]): Seq[Float] = a.map(+_)
  override def applyVar(a: Seq[Float]): Seq[Float]       = a.map(+_)

  override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = Seq.fill(a.size)(+1f)

  override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = Seq.fill(a.size)(1f * b)
  override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (_, y) => y }

  override def gradVar(a: Seq[Float]): Seq[Float] = Seq.fill(a.size)(1f)

}
