package com.kogecoo.scalaad.op.bool


case object Not extends UnaryBooleanOp

case object And extends BinaryBooleanOp

case object Or  extends BinaryBooleanOp


case object AndLeft extends AsymmetricLeftBinaryBooleanOp

case object OrLeft  extends AsymmetricLeftBinaryBooleanOp

case object AndRight extends AsymmetricRightBinaryBooleanOp

case object OrRight  extends AsymmetricRightBinaryBooleanOp
