package scalaad.impl.nd4j.op

import org.nd4j.linalg.api.complex.IComplexNumber


abstract class WhereCondition {

  def op(x: IComplexNumber, y: Double): Boolean

  def op(x: IComplexNumber, y: Float): Boolean

  def op(x: IComplexNumber, y: IComplexNumber): Boolean

  def op(x: Float, y: Float): Boolean

  def op(x: Double, y: Double): Boolean

  def op(x: Double): Boolean

  def op(x: Float): Boolean

  def op(x: IComplexNumber): Boolean

}

