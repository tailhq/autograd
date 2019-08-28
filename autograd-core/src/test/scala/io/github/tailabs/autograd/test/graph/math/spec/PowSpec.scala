package io.github.tailabs.autograd.test.graph.math.spec

import io.github.tailabs.autograd.graph.{Node, pow}
import io.github.tailabs.autograd.rule._
import io.github.tailabs.autograd.test.helper.gen._
import io.github.tailabs.autograd.test.helper.rule.SeqFloatSoftCompareRule
import io.github.tailabs.autograd.test.helper.rule.SeqFloatValueRule.Implicits._
import io.github.tailabs.autograd.test.helper.specgen.{BinaryOpExpectedBehaviorDef, BinaryOpSpec}
import org.scalacheck.Properties

import scala.language.higherKinds


object PowSpecSeqFloat extends Properties("Pow - Seq[Float]") {


  val nodeGen  = new SeqFloatNodeGenWithValueRange(1e-10f, 10f)
  val valueGen = new SeqFloatValueGenWithValueRange(-1e15f, 1e15f)
  val expects = new PowSeqFloatExpectedBehavior
  implicit val compareRule = new SeqFloatSoftCompareRule

  val specGen = new BinaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

  property("scalar ^ scalar       apply") = specGen.applyScalarScalar()
  property("scalar ^ container    apply") = specGen.applyScalarContainer()
  property("scalar ^ var          apply") = specGen.applyScalarVar()
  property("container ^ scalar    apply") = specGen.applyContainerScalar()
  property("container ^ container apply") = specGen.applyContainerContainer()
  property("container ^ var       apply") = specGen.applyContainerVar()
  property("var ^ scalar          apply") = specGen.applyVarScalar()
  property("var ^ container       apply") = specGen.applyVarContainer()
  property("var ^ var             apply") = specGen.applyVarVar()

  property("scalar ^ var          deriv w.r.t. right")   = specGen.derivScalarVarWrtRight()
  property("scalar ^ var          deriv w.r.t. unknown") = specGen.derivScalarVarWrtUnknown()
  property("container ^ var       deriv w.r.t. right")   = specGen.derivContainerVarWrtRight()
  property("container ^ var       deriv w.r.t. unknown") = specGen.derivContainerVarWrtUnknown()
  property("var ^ scalar          deriv w.r.t. left")    = specGen.derivVarScalarWrtLeft()
  property("var ^ scalar          deriv w.r.t. unknown") = specGen.derivVarScalarWrtUnknown()
  property("var ^ container       deriv w.r.t. left")    = specGen.derivVarContainerWrtLeft()
  property("var ^ container       deriv w.r.t. unknown") = specGen.derivVarContainerWrtUnknown()
  property("var ^ var             deriv w.r.t. left")    = specGen.derivVarVarWrtLeft()
  property("var ^ var             deriv w.r.t. right")   = specGen.derivVarVarWrtRight()
  property("var ^ var             deriv w.r.t. unknown") = specGen.derivVarVarWrtUnknown()
  property("var ^ var             deriv w.r.t. self")    = specGen.derivVarVarWrtSelf()

  property("scalar ^ scalar       propagete value")     = specGen.propagateScalarScalarWithNCValue()
  property("scalar ^ scalar       propagete container") = specGen.propagateScalarScalarWithCValue()
  property("scalar ^ container    propagete value")     = specGen.propagateScalarContainerWithNCValue()
  property("scalar ^ container    propagete container") = specGen.propagateScalarContainerWithCValue()
  property("scalar ^ var          propagete value")     = specGen.propagateScalarVarWithNCValue()
  property("scalar ^ var          propagete container") = specGen.propagateScalarVarWithCValue()
  property("container ^ scalar    propagete value")     = specGen.propagateContainerScalarWithNCValue()
  property("container ^ scalar    propagete container") = specGen.propagateContainerScalarWithCValue()
  property("container ^ container propagete value")     = specGen.propagateContainerContainerWithNCValue()
  property("container ^ container propagete container") = specGen.propagateContainerContainerWithCValue()
  property("container ^ var       propagete value")     = specGen.propagateContainerVarWithNCValue()
  property("container ^ var       propagete container") = specGen.propagateContainerVarWithCValue()
  property("var ^ scalar          propagete value")     = specGen.propagateVarScalarWithNCValue()
  property("var ^ scalar          propagete container") = specGen.propagateVarScalarWithCValue()
  property("var ^ container       propagete value")     = specGen.propagateVarContainerWithNCValue()
  property("var ^ container       propagete container") = specGen.propagateVarContainerWithCValue()
  property("var ^ var             propagete value")     = specGen.propagateVarVarWithNCValue()
  property("var ^ var             propagete container") = specGen.propagateVarVarWithCValue()

  property("scalar ^ scalar       grad") = specGen.gradScalarScalar()
  property("scalar ^ container    grad") = specGen.gradScalarContainer()
  property("scalar ^ var          grad") = specGen.gradScalarVar()
  property("container ^ scalar    grad") = specGen.gradContainerScalar()
  property("container ^ container grad") = specGen.gradContainerContainer()
  property("container ^ var       grad") = specGen.gradContainerVar()
  property("var ^ scalar          grad") = specGen.gradVarScalar()
  property("var ^ container       grad") = specGen.gradVarContainer()
  property("var ^ var             grad") = specGen.gradVarVar()

}


class PowSeqFloatExpectedBehavior(implicit vr: ValueRule[Seq, Float]) extends BinaryOpExpectedBehaviorDef[Seq, Float] {

  override val zero = 0f

  val one = 1f

  override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

  def one(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(1f)

  def fpow(x: Float, y: Float): Float = scala.math.pow(x.toDouble, y.toDouble).toFloat
  def flog(x: Float): Float = scala.math.log(x.toDouble).toFloat

  override def op(a: Node[Seq, Float], b: Node[Seq, Float]): Node[Seq, Float] = pow(a, b)

  override def applyScalarScalar(a: Float, b: Float): Float                      = fpow(a, b)
  override def applyScalarContainer(a: Float, b: Seq[Float]): Seq[Float]         = b.map(fpow(a, _))
  override def applyScalarVar(a: Float, b: Seq[Float]): Seq[Float]               = b.map(fpow(a, _))
  override def applyContainerScalar(a: Seq[Float], b: Float): Seq[Float]         = a.map(fpow(_, b))
  override def applyContainerContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => fpow(x, y) }
  override def applyContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float]       = a.zip(b).map { case (x, y) => fpow(x, y) }
  override def applyVarScalar(a: Seq[Float], b: Float): Seq[Float]               = a.map(fpow(_, b))
  override def applyVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float]       = a.zip(b).map { case (x, y) => fpow(x, y) }
  override def applyVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float]             = a.zip(b).map { case (x, y) => fpow(x, y) }


  override def derivScalarVarWrtRight(a: Float, b: Seq[Float]): Seq[Float]         = b.map { y => flog(a) * fpow(a, y)}
  override def derivContainerVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => flog(x) * fpow(x, y) }
  override def derivVarContainerWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float]  = a.zip(b).map { case (x, y) => y * fpow(x, y - 1f) }
  override def derivVarScalarWrtLeft(a: Seq[Float], b: Float): Seq[Float]          = a.map { x => b * fpow(x, b - 1f) }
  override def derivVarVarWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float]        = a.zip(b).map { case (x, y) => y * fpow(x, y - 1f) }
  override def derivVarVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float]       = a.zip(b).map { case (x, y) => flog(x) * fpow(x, y) }
  override def derivVarVarWrtSelf(a: Seq[Float]): Seq[Float]                       = a.map { x => flog(x) * fpow(x, x) + x * fpow(x, x - 1f) }


  override def propagateScalarVarWithNCValue(a: Float, b: Seq[Float], c: Float): Seq[Float]             = b.map { y => c * flog(a) * fpow(a, y)}
  override def propagateScalarVarWithCValue(a: Float, b: Seq[Float], c: Seq[Float]): Seq[Float]         = b.zip(c).map { case (y, z) => z * flog(a) * fpow(a, y)}
  override def propagateContainerVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float]     = a.zip(b).map { case (x, y) => c * flog(x) * fpow(x, y) }
  override def propagateContainerVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c).map { case ((x, y), z) => z * flog(x) * fpow(x, y) }
  override def propagateVarScalarWithCValue(a: Seq[Float], b: Float, c: Seq[Float]): Seq[Float]         = a.zip(c).map { case(x, z) => z * b * fpow(x, b - 1f) }
  override def propagateVarScalarWithNCValue(a: Seq[Float], b: Float, c: Float): Seq[Float]             = a.map { x => c * b * fpow(x, b - 1f) }
  override def propagateVarContainerWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float]     = a.zip(b).map { case (x, y) => c * y * fpow(x, y - 1f) }
  override def propagateVarContainerWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c).map { case ((x, y), z) => z * y * fpow(x, y - 1f) }
  override def propagateVarVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float]           = a.zip(b).map { case (x, y) => c * (y * fpow(x, y - 1f) + flog(x) * fpow(x, y)) }
  override def propagateVarVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float]       = a.zip(b).zip(c).map { case ((x, y), z) => z * (y * fpow(x, y - 1f) + flog(x) * fpow(x, y)) }


  override def gradScalarVar(a: Float, b: Seq[Float]): Seq[Float]         = b.map { y => flog(a) * fpow(a, y)}
  override def gradContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => flog(x) * fpow(x, y) }
  override def gradVarScalar(a: Seq[Float], b: Float): Seq[Float]         = a.map { x => b * fpow(x, b - 1f) }
  override def gradVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => y * fpow(x, y - 1f) }
  override def gradVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float]       = a.zip(b).map { case (x, y) => y * fpow(x, y - 1f) + flog(x) * fpow(x, y) }

}
