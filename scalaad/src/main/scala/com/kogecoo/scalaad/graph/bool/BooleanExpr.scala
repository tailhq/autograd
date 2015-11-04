package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph.{B, Expr}


trait BooleanExpr extends Expr {

  def eval[R](implicit E: Eval[BooleanExpr, R]): R = E.eval(this)

}


object BooleanExpr {

  implicit class RichValueExpr(val self: B) extends AnyVal {

    def :&(rhs: B): B = And(self, rhs)
    def :|(rhs: B): B = Or(self, rhs)

    def unary_!(): B = Not(self)

  }

}

