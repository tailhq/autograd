/*package com.kogecoo.scalaad.breeze.test

import breeze.linalg.DenseMatrix
import com.kogecoo.scalaad.graph.{Node, dot}
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.breeze.test.helper.gen._
import com.kogecoo.scalaad.breeze.test.helper.rule.DenseMatrixDoubleSoftCompareRule
import com.kogecoo.scalaad.breeze.BreezeRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.BinaryOpExpectedBehaviorDef
import org.scalacheck.Properties

import scala.language.higherKinds


object DotSpecDenseMatrixDouble extends Properties("Dot - DenseMatrix[Double]") {

  val shapeGen = new MMulMatrixShapeGen(1, 1, 20, 20)
  val nodeGen  = new DenseMatrixNodeGen(-1e50, 1e50)
  val valueGen = new DenseMatrixValueGen(-1e50, 1e50)
  val expects = new DotDenseMatrixDoubleExpectedBehavior
  implicit val compareRule = new DenseMatrixDoubleSoftCompareRule

  val specGen = new MatrixDotBinaryOpSpec[DenseMatrix, Double](expects, shapeGen, nodeGen, valueGen)

  val r = (x: Double) => !x.equals(Double.NaN) && !x.equals(Double.PositiveInfinity) && !x.equals(Double.NegativeInfinity)

  property("scalar dot scalar       apply") = specGen.applyScalarScalar(r, r)
  property("scalar dot container    apply") = specGen.applyScalarContainer(r, r)
  property("scalar dot var          apply") = specGen.applyScalarVar(r, r)
  property("container dot scalar    apply") = specGen.applyContainerScalar(r, r)
  property("container dot container apply") = specGen.applyContainerContainer(r, r)
  property("container dot var       apply") = specGen.applyContainerVar(r, r)
  property("var dot scalar          apply") = specGen.applyVarScalar(r, r)
  property("var dot container       apply") = specGen.applyVarContainer(r, r)
  property("var dot var             apply") = specGen.applyVarVar(r, r)

  property("scalar dot var          deriv w.r.t. right")   = specGen.derivScalarVarWrtRight(r, r)
  property("scalar dot var          deriv w.r.t. unknown") = specGen.derivScalarVarWrtUnknown(r, r)
  property("container dot var       deriv w.r.t. right")   = specGen.derivContainerVarWrtRight(r, r)
  property("container dot var       deriv w.r.t. unknown") = specGen.derivContainerVarWrtUnknown(r, r)
  property("var dot scalar          deriv w.r.t. left")    = specGen.derivVarScalarWrtLeft(r, r)
  property("var dot scalar          deriv w.r.t. unknown") = specGen.derivVarScalarWrtUnknown(r, r)
  property("var dot container       deriv w.r.t. left")    = specGen.derivVarContainerWrtLeft(r, r)
  property("var dot container       deriv w.r.t. unknown") = specGen.derivVarContainerWrtUnknown(r, r)
  property("var dot var             deriv w.r.t. left")    = specGen.derivVarVarWrtLeft(r, r)
  property("var dot var             deriv w.r.t. right")   = specGen.derivVarVarWrtRight(r, r)
  property("var dot var             deriv w.r.t. unknown") = specGen.derivVarVarWrtUnknown(r, r)
  property("var dot var             deriv w.r.t. self")    = specGen.derivVarVarWrtSelf(r)

  property("scalar dot scalar       propagete value")     = specGen.propagateScalarScalarWithNCValue(r, r, r)
  property("scalar dot scalar       propagete container") = specGen.propagateScalarScalarWithCValue(r, r, r)
  property("scalar dot container    propagete value")     = specGen.propagateScalarContainerWithNCValue(r, r, r)
  property("scalar dot container    propagete container") = specGen.propagateScalarContainerWithCValue(r, r, r)
  property("scalar dot var          propagete value")     = specGen.propagateScalarVarWithNCValue(r, r, r)
  property("scalar dot var          propagete container") = specGen.propagateScalarVarWithCValue(r, r, r)
  property("container dot scalar    propagete value")     = specGen.propagateContainerScalarWithNCValue(r, r, r)
  property("container dot scalar    propagete container") = specGen.propagateContainerScalarWithCValue(r, r, r)
  property("container dot container propagete value")     = specGen.propagateContainerContainerWithNCValue(r, r, r)
  property("container dot container propagete container") = specGen.propagateContainerContainerWithCValue(r, r, r)
  property("container dot var       propagete value")     = specGen.propagateContainerVarWithNCValue(r, r, r)
  property("container dot var       propagete container") = specGen.propagateContainerVarWithCValue(r, r, r)
  property("var dot scalar          propagete value")     = specGen.propagateVarScalarWithNCValue(r, r, r)
  property("var dot scalar          propagete container") = specGen.propagateVarScalarWithCValue(r, r, r)
  property("var dot container       propagete value")     = specGen.propagateVarContainerWithNCValue(r, r, r)
  property("var dot container       propagete container") = specGen.propagateVarContainerWithCValue(r, r, r)
  property("var dot var             propagete value")     = specGen.propagateVarVarWithNCValue(r, r, r)
  property("var dot var             propagete container") = specGen.propagateVarVarWithCValue(r, r, r)

  property("scalar dot scalar       grad") = specGen.gradScalarScalar(r, r)
  property("scalar dot container    grad") = specGen.gradScalarContainer(r, r)
  property("scalar dot var          grad") = specGen.gradScalarVar(r, r)
  property("container dot scalar    grad") = specGen.gradContainerScalar(r, r)
  property("container dot container grad") = specGen.gradContainerContainer(r, r)
  property("container dot var       grad") = specGen.gradContainerVar(r, r)
  property("var dot scalar          grad") = specGen.gradVarScalar(r, r)
  property("var dot container       grad") = specGen.gradVarContainer(r, r)
  property("var dot var             grad") = specGen.gradVarVar(r, r)

}


class DotDenseMatrixDoubleExpectedBehavior(implicit vr: ValueRule[DenseMatrix, Double]) extends BinaryOpExpectedBehaviorDef[DenseMatrix, Double] {

  type M = DenseMatrix[Double]

  type T = Double

  val one = 1.0

  def one(shape: M): M = DenseMatrix.ones[T](shape.rows, shape.cols)

  def one(a: M, b: M): M = DenseMatrix.ones[T](a.rows, b.cols)

  override val zero = 0.0

  override def zero(shape: M): M = DenseMatrix.zeros[T](shape.rows, shape.cols)

  def zero(a: M, b: M): M = DenseMatrix.zeros[T](a.rows, b.cols)

  override def op(a: Node[DenseMatrix, T], b: Node[DenseMatrix, T]): Node[DenseMatrix, T] = dot(a, b)

  override def applyScalarScalar(a: T, b: T): T       = a * b
  override def applyScalarContainer(a: T, b: M): M    = a * b
  override def applyScalarVar(a: T, b: M): M          = a * b
  override def applyContainerScalar(a: M, b: T): M    = a * b
  override def applyContainerContainer(a: M, b: M): M = a * b
  override def applyContainerVar(a: M, b: M): M       = a * b
  override def applyVarScalar(a: M, b: T): M          = a * b
  override def applyVarContainer(a: M, b: M): M       = a * b
  override def applyVarVar(a: M, b: M): M             = a * b

  override def derivScalarVarWrtRight(a: T, b: M): M            = a * one(b)
  override def derivContainerVarWrtRight(a: M, b: M): M         = a * one(b)
  override def derivVarContainerWrtLeft(a: M, b: M): M          = one(a) * b
  override def derivVarScalarWrtLeft(a: M, b: T): M             = one(a) * b
  override def derivVarVarWrtLeft(a: M, b: M): M                = one(a) * b
  override def derivVarVarWrtRight(a: M, b: M): M               = a * one(b)
  override def derivVarVarWrtSelf(a: M): M                      = a * one(a.t) + one(a) * a.t

  override def propagateScalarVarWithNCValue(a: T, b: M, c: T): M          = a * b * c
  override def propagateScalarVarWithCValue(a: T, b: M, c: M): M           = c * (b :* a)
  override def propagateScalarContainerWithNCValue(a: T, b: M, c: T): M    = zero(b)
  override def propagateScalarContainerWithCValue(a: T, b: M, c: M): M     = zero(c)
  override def propagateContainerContainerWithNCValue(a: M, b: M, c: T): M = zero(a, b)
  override def propagateContainerContainerWithCValue(a: M, b: M, c: M): M  = zero(c)
  override def propagateContainerVarWithNCValue(a: M, b: M, c: T): M       = (a * one(b)) :* c
  override def propagateContainerVarWithCValue(a: M, b: M, c: M): M        = c * a
  override def propagateVarScalarWithCValue(a: M, b: T, c: M): M           = one(a) * b
  override def propagateVarScalarWithNCValue(a: M, b: T, c: T): M          = DenseMatrix.fill(a.rows, a.cols)(b) :* c
  override def propagateVarContainerWithNCValue(a: M, b: M, c: T): M       = c * b
  override def propagateVarContainerWithCValue(a: M, b: M, c: M): M        = c * b
  override def propagateVarVarWithNCValue(a: M, b: M, c: T): M             = a * c + c * b
  override def propagateVarVarWithCValue(a: M, b: M, c: M): M              = a * c + c * b


  override def gradScalarVar(a: T, b: M): M          = a * one(b)
  override def gradContainerVar(a: M, b: M): M       = a * one(a, b)
  override def gradContainerContainer(a: M, b: M): M = a.t * zero(a, b) + zero(a, b) * b.t
  override def gradVarScalar(a: M, b: T): M          = one(a) * b
  override def gradVarContainer(a: M, b: M): M       = one(a, b) * b
  override def gradVarVar(a: M, b: M): M             = (one(a, b) * b) + (a * one(a, b))

}
*/
