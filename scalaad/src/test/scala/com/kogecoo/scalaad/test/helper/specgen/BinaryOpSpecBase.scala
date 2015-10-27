package com.kogecoo.scalaad.test.helper.specgen

import org.scalacheck.Prop

import scala.language.higherKinds


trait BinaryOpSpecBase[U[_], T] {

  type Restriction = T => Boolean

  lazy val default: Restriction = (_: T) => true

  def applyScalarScalar(r1: Restriction = default, r2: Restriction = default): Prop

  def applyScalarContainer(r1: Restriction = default, r2: Restriction = default): Prop

  def applyScalarVar(r1: Restriction = default, r2: Restriction = default): Prop

  def applyContainerScalar(r1: Restriction = default, r2: Restriction = default): Prop

  def applyContainerContainer(r1: Restriction = default, r2: Restriction = default): Prop

  def applyContainerVar(r1: Restriction = default, r2: Restriction = default): Prop

  def applyVarScalar(r1: Restriction = default, r2: Restriction = default): Prop

  def applyVarContainer(r1: Restriction = default, r2: Restriction = default): Prop

  def applyVarVar(r1: Restriction = default, r2: Restriction = default): Prop


  def derivScalarVarWrtRight(r1: Restriction = default, r2: Restriction = default): Prop

  def derivScalarVarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def derivContainerVarWrtRight(r1: Restriction = default, r2: Restriction = default): Prop

  def derivContainerVarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def derivVarScalarWrtLeft(r1: Restriction = default, r2: Restriction = default): Prop

  def derivVarScalarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def derivVarContainerWrtLeft(r1: Restriction = default, r2: Restriction = default): Prop

  def derivVarContainerWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def derivVarVarWrtLeft(r1: Restriction = default, r2: Restriction = default): Prop

  def derivVarVarWrtRight(r1: Restriction = default, r2: Restriction = default): Prop

  def derivVarVarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def derivVarVarWrtSelf(r: Restriction = default): Prop

  def propagateScalarScalarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateScalarContainerWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateScalarVarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateContainerScalarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateContainerContainerWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateContainerVarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateVarScalarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateVarContainerWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateVarVarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateScalarScalarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateScalarContainerWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateScalarVarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateContainerScalarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateContainerContainerWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateContainerVarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateVarScalarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateVarContainerWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def propagateVarVarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop

  def gradScalarScalar(r1: Restriction = default, r2: Restriction = default): Prop

  def gradScalarContainer(r1: Restriction = default, r2: Restriction = default): Prop

  def gradScalarVar(r1: Restriction = default, r2: Restriction = default): Prop

  def gradContainerScalar(r1: Restriction = default, r2: Restriction = default): Prop

  def gradContainerContainer(r1: Restriction = default, r2: Restriction = default): Prop

  def gradContainerVar(r1: Restriction = default, r2: Restriction = default): Prop

  def gradVarScalar(r1: Restriction = default, r2: Restriction = default): Prop

  def gradVarContainer(r1: Restriction = default, r2: Restriction = default): Prop

  def gradVarVar(r1: Restriction = default, r2: Restriction = default): Prop

}
