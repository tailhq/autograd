package com.kogecoo.scalaad.algorithm

import scala.Predef.{any2stringadd => _}


/*trait Reverse[N, G] {

  def reverse(n: N, g: G): Grad

}*/

/**
  * Supported combinations of Node's tensor order
  **
  *1st:  Node order which will be differentiated
  *2nd: propagating (inExprediate) gradient.
  **
  *0 0
  *0 1
  *0 2
  *1 0
  *1 1
  *1 2
  *2 0
  *2 1
  *2 2
  *
  **/
/*
object Reverse {


  implicit def reverse00: Reverse[V0, V0] = new Reverse[V0, V0] {

    private[this] type N = V0
    private[this] type G = V0

    def reverse(n: N, g: G): Grad = n match {

      case _: Var0    => Grad(n, g)
      case _: ArbVar0 => Grad(n, g)
      case _: Zero0   => Grad.empty
      case _: One0    => Grad.empty
      case _: Half0   => Grad.empty
      case _: Const0  => Grad.empty

      case Apply0(v, op) => op match {
        case Pos0 => v.reverse[G](+g)
        case Neg0 => v.reverse[G](-g)

        case Sin0 => v.reverse[G](g * Cos0(v))
        case Cos0 => v.reverse[G](-g * Sin0(v))
        case Tan0 => v.reverse[G](g * (One0() + (Tan0(v) * Tan0(v))))

        case Asin0 => v.reverse[G](g *  (One0() / Sqrt0(One0() - (v * v))))
        case Acos0 => v.reverse[G](g * -(One0() / Sqrt0(One0() - (v * v))))
        case Atan0 => v.reverse[G](g *  (One0() / (One0() + (v * v))))

        case Sinh0 => v.reverse[G](g * Cosh0(v))
        case Cosh0 => v.reverse[G](g * Sinh0(v))
        case Tanh0 => v.reverse[G](g * (One0() - (Tanh0(v) * Tanh0(v))))

        case Ln0      => v.reverse[G](g / v)
        case Exp0     => v.reverse[G](g * Exp0(v))
        case Sqrt0    => v.reverse[G](g * (Half0() / Sqrt0(v)))
        case Abs0     => Grad.where(Gt00(v, Zero0()), v.reverse[G](g), v.reverse[G](-g))
      }
      case Apply00(l, r, op) => op match {
        case Add00 => l.reverse[G](g) ++ r.reverse[G](g)
        case Sub00 => l.reverse[G](g) ++ r.reverse[G](-g)
        case Mul00 => l.reverse[G](g * r) ++ r.reverse[G](l * g)
        case Div00 => l.reverse[G](g / r) ++ r.reverse[G]((-l * g) / (r * r))

        case Pow00 => {
          val lg = l.reverse[G]((g * r) * Pow00(l, r - One0()))
          val rg = r.reverse[G](g * (Ln0(l) * Pow00(l, r)))
          lg ++ rg
        }

        // Experimental
        case Max00 => Grad.where(Gt00(l, r), l.reverse[G](g), r.reverse[G](g))
        case Min00 => Grad.where(Lt00(l, r), l.reverse[G](g), r.reverse[G](g))

      }
      /*case Dot01(l, r) => l.reverse[G](Dot01(g, r)) ++ r.reverse[G](l * g)
      case Dot10(l, r) => l.reverse[G](g * r)       ++ r.reverse[G](Dot10(l, g))
      case Dot11(l, r) => l.reverse[G](Dot01(g, r)) ++ r.reverse[G](Dot10(l, g))*/
    }
  }

  /*
  implicit def reverse01: Reverse[V0, V1] = new Reverse[V0, V1] {

    private[this] type N = V0
    private[this] type G = V1
    private[this] type G0 = V0

    def reverse(n: N, g: G): Grad = n match {

      case _: Var0    => Grad(n, g)
      case _: ArbVar0 => Grad(n, g)
      case _: Zero0   => Grad.empty
      case _: Half0   => Grad.empty
      case _: One0    => Grad.empty
      case _: Const0  => Grad.empty

      case Pos0(v) => v.reverse[G](+g)
      case Neg0(v) => v.reverse[G](-g)

      case Add00(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Sub00(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Mul00(l, r) => l.reverse[G](g :* r) ++ r.reverse[G](l :* g)
      case Div00(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G]((-l :* g) :/ (r * r))

      case Sin0(v) => v.reverse[G](g :* Cos0(v))
      case Cos0(v) => v.reverse[G](-g :* Sin0(v))
      case Tan0(v) => v.reverse[G](g :* (One0() + (Tan0(v) * Tan0(v))))

      case Asin0(v) => v.reverse[G](g :*  (One0() / Sqrt0(One0() - (v * v))))
      case Acos0(v) => v.reverse[G](g :* -(One0() / Sqrt0(One0() - (v * v))))
      case Atan0(v) => v.reverse[G](g :*  (One0() / (One0() + (v * v))))

      case Sinh0(v) => v.reverse[G](g :* Cosh0(v))
      case Cosh0(v) => v.reverse[G](g :* Sinh0(v))
      case Tanh0(v) => v.reverse[G](g :* (One0() - (Tanh0(v) * Tanh0(v))))

      case Ln0(v)      => v.reverse[G](g :/ v)
      case Exp0(v)     => v.reverse[G](g :* Exp0(v))
      case Sqrt0(v)    => v.reverse[G](g :* (Half0() / Sqrt0(v)))
      case Pow00(l, r) => {
        val lhs = l.reverse[G]((g :* r) :* Pow00(l, r - One0()))
        val rhs = r.reverse[G](g :* (Ln0(l) * Pow00(l, r)))
        lhs ++ rhs
      }

      // Experimental
      case Abs0(v)     => Grad.where(Gt00(v, Zero0()), v.reverse[G](g),  v.reverse[G](-g))
      case Max00(l, r) => Grad.where(Gt00(l, r),       l.reverse[G](g),  r.reverse[G]( g))
      case Min00(l, r) => Grad.where(Lt00(l, r),       l.reverse[G](g),  r.reverse[G]( g))

      case Dot01(l, r) => l.reverse[G0](Dot11(g, r)) ++ r.reverse[G0](Dot01(l, g))
      case Dot10(l, r) => l.reverse[G0](Dot10(g, r)) ++ r.reverse[G0](Dot11(l, g))
      case Dot11(l, r) => l.reverse[G0](Dot11(g, r)) ++ r.reverse[G0](Dot11(l, g))
    }
  }

  implicit def reverse02: Reverse[V0, V2] = new Reverse[V0, V2] {

    private[this] type N  = V0
    private[this] type G  = V2

    def reverse(n: N, g: G): Grad = n match {

      case _: Var0    => Grad(n, g)
      case _: ArbVar0 => Grad(n, g)
      case _: Zero0  => Grad.empty
      case _: Half0  => Grad.empty
      case _: One0   => Grad.empty
      case _: Const0 => Grad.empty

      case Pos0(v) => v.reverse[G](+g)
      case Neg0(v) => v.reverse[G](-g)

      case Add00(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Sub00(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Mul00(l, r) => l.reverse[G](g :* r) ++ r.reverse[G](l :* g)
      case Div00(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G]((-l :* g) :/ (r * r))

      case Sin0(v) => v.reverse[G](g :* Cos0(v))
      case Cos0(v) => v.reverse[G](-g :* Sin0(v))
      case Tan0(v) => v.reverse[G](g :* (One0() + (Tan0(v) * Tan0(v))))

      case Asin0(v) => v.reverse[G](g :*  (One0() / Sqrt0(One0() - (v * v))))
      case Acos0(v) => v.reverse[G](g :* -(One0() / Sqrt0(One0() - (v * v))))
      case Atan0(v) => v.reverse[G](g :*  (One0() / (One0() + (v * v))))

      case Sinh0(v) => v.reverse[G](g :* Cosh0(v))
      case Cosh0(v) => v.reverse[G](g :* Sinh0(v))
      case Tanh0(v) => v.reverse[G](g :* (One0() - (Tanh0(v) * Tanh0(v))))

      case Ln0(v)      => v.reverse[G](g :/ v)
      case Exp0(v)     => v.reverse[G](g :* Exp0(v))
      case Sqrt0(v)    => v.reverse[G](g :* (Half0() / Sqrt0(v)))
      case Pow00(l, r) => {
        val lhs = l.reverse[G]((g :* r) :* Pow00(l, r - One0()))
        val rhs = r.reverse[G](g :* (Ln0(l) * Pow00(l, r)))
        lhs ++ rhs
      }

      // Experimental
      case Abs0(v)     => Grad.where(Gt00(v, Zero0()), v.reverse[G](g), v.reverse[G](-g))
      case Max00(l, r) => Grad.where(Gt00(l, r),       l.reverse[G](g), r.reverse[G]( g))
      case Min00(l, r) => Grad.where(Lt00(l, r),       l.reverse[G](g), r.reverse[G]( g))
    }
  }

  implicit def reverse10: Reverse[V1, V0] = new Reverse[V1, V0] {

    private[this] type N  = V1
    private[this] type G  = V0
    private[this] type G1 = V1

    def reverse(n: N, g: G): Grad = n match {

      case _: Var1    => Grad(n, g :* One1(n))
      case _: ArbVar1 => Grad(n, g :* One1(n))
      case _: Zero1   => Grad.empty
      case _: Half1   => Grad.empty
      case _: One1    => Grad.empty
      case _: Const1  => Grad.empty

      case Pos1(v) => v.reverse[G](+g)
      case Neg1(v) => v.reverse[G](-g)
      case Transpose1(v) => v.reverse[G](g)

      case Add01(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add10(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add11(l, r) => l.reverse[G](g) ++ r.reverse[G](g)

      case Sub01(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub10(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub11(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)

      case Mul01(l, r) => l.reverse[G1](g :* r) ++ r.reverse[G](l  * g)
      case Mul10(l, r) => l.reverse[G](g  * r) ++ r.reverse[G1](l :* g)
      case Mul11(l, r) => l.reverse[G1](g :* r) ++ r.reverse[G1](l :* g)

      case Div01(l, r) => l.reverse[G1](g :/ r) ++ r.reverse[G1](-(l  * g) :/ (r * r))
      case Div10(l, r) => l.reverse[G](g  / r) ++ r.reverse[G1](-(l :* g) :/ (r * r))
      case Div11(l, r) => l.reverse[G1](g :/ r) ++ r.reverse[G1](-(l :* g)  / (r * r))

      case Sin1(v) => v.reverse[G1](g :* Cos1(v))
      case Cos1(v) => v.reverse[G1](-g :* Sin1(v))
      case Tan1(v) => v.reverse[G1](g :* (One1(v) + (Tan1(v) * Tan1(v))))

      case Asin1(v) => v.reverse[G1](g :*  (One1(v) / Sqrt1(One1(v) - (v * v))))
      case Acos1(v) => v.reverse[G1](g :* -(One1(v) / Sqrt1(One1(v) - (v * v))))
      case Atan1(v) => v.reverse[G1](g :*  (One1(v) / (One1(v) + (v * v))))

      case Sinh1(v) => v.reverse[G1](g :* Cosh1(v))
      case Cosh1(v) => v.reverse[G1](g :* Sinh1(v))
      case Tanh1(v) => v.reverse[G1](g :* (One1(v) - (Tanh1(v) * Tanh1(v))))

      case Ln1(v)      => v.reverse[G1](g :/ v)
      case Exp1(v)     => v.reverse[G1](g :* Exp1(v))
      case Sqrt1(v)    => v.reverse[G1](g :* (Half1(v) / Sqrt1(v)))
      case Pow01(l, r) => {
        val lhs = l.reverse[G1]((g :* r) * Pow01(l, r :- One0()))
        val rhs = r.reverse[G1](g :* (Ln0(l) :* Pow01(l, r)))
        lhs ++ rhs
      }
      case Pow10(l, r) => {
        val lhs = l.reverse[G1]((g * r) :* Pow10(l, r  - One0()))
        val rhs = r.reverse[G1](g :* (Ln1(l) * Pow10(l, r)))
        lhs ++ rhs
      }
      case Pow11(l, r) => {
        val lhs = l.reverse[G1]((g :* r) * Pow11(l, r :- One0()))
        val rhs = r.reverse[G1](g :* (Ln1(l) * Pow11(l, r)))
        lhs ++ rhs
      }

      // Experimental
      case VecFill(v, s) => v.reverse[G1](VecFill(g, s))

      case Abs1(v)     => Grad.where(Gt10(v, Zero0()), v.reverse[G](g), v.reverse[G](-g))
      case Max11(l, r) => Grad.where(Gt11(l, r),       l.reverse[G](g), r.reverse[G]( g))
      case Min11(l, r) => Grad.where(Lt11(l, r),       l.reverse[G](g), r.reverse[G]( g))
    }
  }

  implicit def reverse11: Reverse[V1, V1] = new Reverse[V1, V1] {

    private[this] type N = V1
    private[this] type G = V1
    private[this] type G2 = V2

    def reverse(n: N, g: G): Grad = n match {

      // FIXME: former g * N1 calculation foreach case for running shape check
      case _: Var1    => g * One1(n);  Grad(n, g)
      case _: ArbVar1 => g * One1(n);  Grad(n, g)
      case _: Zero1   => g * Zero1(n); Grad.empty
      case _: Half1   => g * Zero1(n); Grad.empty
      case _: One1    => g * Zero1(n); Grad.empty
      case _: Const1  => g * Zero1(n); Grad.empty

      case Pos1(v) => v.reverse[G](+g)
      case Neg1(v) => v.reverse[G](-g)
      case Transpose1(v) => v.reverse[G](g.T)

      case Add01(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add10(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add11(l, r) => l.reverse[G](g) ++ r.reverse[G](g)

      case Sub01(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub10(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub11(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)

      case Mul01(l, r) => l.reverse[G](g  * r) ++ r.reverse[G](l :* g)
      case Mul10(l, r) => l.reverse[G](g :* r) ++ r.reverse[G](l  * g)
      case Mul11(l, r) => l.reverse[G](g  * r) ++ r.reverse[G](l  * g)

      case Div01(l, r) => l.reverse[G](g  / r) ++ r.reverse[G]((-l :* g)  / (r * r))
      case Div10(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G]((-l  * g) :/ (r * r))
      case Div11(l, r) => l.reverse[G](g  / r) ++ r.reverse[G]((-l  * g)  / (r * r))

      case Sin1(v) => v.reverse[G](g * Cos1(v))
      case Cos1(v) => v.reverse[G](-g * Sin1(v))
      case Tan1(v) => v.reverse[G](g * (One1(v) + (Tan1(v) * Tan1(v))))

      case Asin1(v) => v.reverse[G](g *  (One1(v) / Sqrt1(One1(v) - (v * v))))
      case Acos1(v) => v.reverse[G](g * -(One1(v) / Sqrt1(One1(v) - (v * v))))
      case Atan1(v) => v.reverse[G](g *  (One1(v) / (One1(v) + (v * v))))

      case Sinh1(v) => v.reverse[G](g * Cosh1(v))
      case Cosh1(v) => v.reverse[G](g * Sinh1(v))
      case Tanh1(v) => v.reverse[G](g * (One1(v) - (Tanh1(v) * Tanh1(v))))

      case Ln1(v)      => v.reverse[G](g / v)
      case Exp1(v)     => v.reverse[G](g * Exp1(v))
      case Sqrt1(v)    => v.reverse[G](g * (Half1(v) / Sqrt1(v)))
      case Pow01(l, r) => {
        val lhs = l.reverse[G]((g  * r) * Pow01(l, r :- One0()))
        val rhs = r.reverse[G](g * (Ln0(l) :* Pow01(l, r)))
        lhs ++ rhs
      }
      case Pow10(l, r) => {
        val lhs = l.reverse[G]((g :* r) * Pow10(l, r  - One0()))
        val rhs = r.reverse[G](g * (Ln1(l) * Pow10(l, r)))
        lhs ++ rhs
      }
      case Pow11(l, r) => {
        val lhs = l.reverse[G]((g * r) * Pow11(l, r :- One0()))
        val rhs = r.reverse[G](g * (Ln1(l) * Pow11(l, r)))
        lhs ++ rhs
      }

      // Experimental
      case VecFill(v, s) => v.reverse[G](g)

      case Abs1(v)     => Grad.where(Gt10(v, Zero0()), v.reverse[G](g), v.reverse[G](-g))
      case Max11(l, r) => Grad.where(Gt11(l, r),       l.reverse[G](g), r.reverse[G]( g))
      case Min11(l, r) => Grad.where(Lt11(l, r),       l.reverse[G](g), r.reverse[G]( g))

      case MatMulR12(l, r) if g.shape.transposed => l.reverse[G](MatMulR12(g  , r)) ++ r.reverse[V0](Dot11(l, g))
      case MatMulR12(l, r)                       => l.reverse[G](MatMulR12(g.T, r)) ++ r.reverse[V0](Dot11(l, g))
      case MatMul2C1(l, r) if g.shape.transposed => l.reverse[V0](Dot11(g, r))      ++ r.reverse[G](MatMul2C1(l, g.T))
      case MatMul2C1(l, r)                       => l.reverse[V0](Dot11(g, r))      ++ r.reverse[G](MatMul2C1(l, g))
    }
  }

  implicit def reverse12: Reverse[V1, V2] = new Reverse[V1, V2] {

    private[this] type N = V1
    private[this] type G = V2

    def reverse(n: N, g: G): Grad = n match {

      // FIXME: former g * N1 calculation foreach case for running shape check
      case _: Var1    => g :* One1(n);  Grad(n, g)
      case _: ArbVar1 => g :* One1(n);  Grad(n, g)
      case _: Zero1   => g :* Zero1(n); Grad.empty
      case _: Half1   => g :* Zero1(n); Grad.empty
      case _: One1    => g :* Zero1(n); Grad.empty
      case _: Const1  => g :* Zero1(n); Grad.empty

      case Pos1(v) => v.reverse[G](+g)
      case Neg1(v) => v.reverse[G](-g)
      case Transpose1(v) => v.reverse[G](g.T)

      case Add01(l, r) => l.reverse[G](g) ++ r.reverse[G] (g)
      case Add10(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add11(l, r) => l.reverse[G](g) ++ r.reverse[G] (g)

      case Sub01(l, r) => l.reverse[G](g) ++ r.reverse[G] (-g)
      case Sub10(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub11(l, r) => l.reverse[G](g) ++ r.reverse[G] (-g)

      case Mul01(l, r) => l.reverse[G](g :* r) ++ r.reverse[G](l :* g)
      case Mul10(l, r) => l.reverse[G](g :* r) ++ r.reverse[G](l :* g)
      case Mul11(l, r) => l.reverse[G](g :* r) ++ r.reverse[G](l :* g)

      case Div01(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G]((-l :* g) :/ (r * r))
      case Div10(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G]((-l :* g) :/ (r * r))
      case Div11(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G]((-l :* g) :/ (r * r))

      case Sin1(v) => v.reverse[G](g :* Cos1(v))
      case Cos1(v) => v.reverse[G](-g :* Sin1(v))
      case Tan1(v) => v.reverse[G](g :* (One1(v) + (Tan1(v) * Tan1(v))))

      case Asin1(v) => v.reverse[G](g :*  (One1(v) / Sqrt1(One1(v) - (v * v))))
      case Acos1(v) => v.reverse[G](g :* -(One1(v) / Sqrt1(One1(v) - (v * v))))
      case Atan1(v) => v.reverse[G](g :*  (One1(v) / (One1(v) + (v * v))))

      case Sinh1(v) => v.reverse[G](g :* Cosh1(v))
      case Cosh1(v) => v.reverse[G](g :* Sinh1(v))
      case Tanh1(v) => v.reverse[G](g :* (One1(v) - (Tanh1(v) * Tanh1(v))))

      case Ln1(v)      => v.reverse[G](g :/ v)
      case Exp1(v)     => v.reverse[G](g :* Exp1(v))
      case Sqrt1(v)    => v.reverse[G](g :* (Half1(v) / Sqrt1(v)))
      case Pow01(l, r) => {
        val lhs = l.reverse[G]((g :* r) :* Pow01(l, r :- One0()))
        val rhs = r.reverse[G](g :* (Ln0(l) :* Pow01(l, r)))
        lhs ++ rhs
      }
      case Pow10(l, r) => {
        val lhs = l.reverse[G]((g :* r) :* Pow10(l, r  - One0()))
        val rhs = r.reverse[G](g :* Ln1(l) :* Pow10(l, r))
        lhs ++ rhs
      }
      case Pow11(l, r) => {
        val lhs = l.reverse[G](g :* r :* Pow11(l, r :- One0()))
        val rhs = r.reverse[G](g :* Ln1(l) * Pow11(l, r))
        lhs ++ rhs
      }

      // Experimental
      case Abs1(v)     => Grad.where(Gt10(v, Zero0()), v.reverse[G](g), v.reverse[G](-g))
      case Max11(l, r) => Grad.where(Gt11(l, r),       l.reverse[G](g), r.reverse[G]( g))
      case Min11(l, r) => Grad.where(Lt11(l, r),       l.reverse[G](g), r.reverse[G]( g))

      case MatMulR12(l, r) => l.reverse[G](MatMul22(g, r))  ++ r.reverse[V1](MatMulR12(l, g.T))
      case MatMul2C1(l, r) => l.reverse[V1](MatMul2C1(g, r)) ++ r.reverse[G](MatMul22(l, g.T))

      case VecFill(v, s) => v.reverse[G](g)
    }
  }

  implicit def reverse20: Reverse[V2, V0] = new Reverse[V2, V0] {

    private[this] type N  = V2
    private[this] type G  = V0
    private[this] type G2 = V2

    def reverse(n: N, g: G): Grad = n match {
      // Leaf nodes
      case _: Var2    => Grad(n, g :* One2(n))
      case _: ArbVar2 => Grad(n, g :* One2(n))
      case _: Zero2   => Grad.empty
      case _: Half2   => Grad.empty
      case _: One2    => Grad.empty
      case _: Const2  => Grad.empty

      case Pos2(v) => v.reverse[G](+g)
      case Neg2(v) => v.reverse[G](-g)
      case Transpose2(v) => v.reverse[G](g)

      case Add02(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add20(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add22(l, r) => l.reverse[G](g) ++ r.reverse[G](g)

      case Sub02(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub20(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub22(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)

      case Mul02(l, r) => l.reverse[G2](g :* r) ++ r.reverse[G](l  * g)
      case Mul20(l, r) => l.reverse[G](g  * r) ++ r.reverse[G2](l :* g)
      case Mul22(l, r) => l.reverse[G2](g :* r) ++ r.reverse[G2](l :* g)

      case Div02(l, r) => l.reverse[G2](g :/ r) ++ r.reverse[G2]((-l  * g) :/ (r * r))
      case Div20(l, r) => l.reverse[G](g  / r) ++ r.reverse[G2]((-l :* g) :/ (r * r))
      case Div22(l, r) => l.reverse[G2](g :/ r) ++ r.reverse[G2]((-l :* g)  / (r * r))

      case Sin2(v) => v.reverse[G2](g :* Cos2(v))
      case Cos2(v) => v.reverse[G2](-g :* Sin2(v))
      case Tan2(v) => v.reverse[G2](g :* (One2(v.shape) + (Tan2(v) * Tan2(v))))

      case Asin2(v) => v.reverse[G2](g :*  (One2(v.shape) / Sqrt2(One2(v.shape) - (v * v))))
      case Acos2(v) => v.reverse[G2](g :* -(One2(v.shape) / Sqrt2(One2(v.shape) - (v * v))))
      case Atan2(v) => v.reverse[G2](g :*  (One2(v.shape) / (One2(v.shape) + (v * v))))

      case Sinh2(v) => v.reverse[G2](g :* Cosh2(v))
      case Cosh2(v) => v.reverse[G2](g :* Sinh2(v))
      case Tanh2(v) => v.reverse[G2](g :* (One2(v.shape) - (Tanh2(v) * Tanh2(v))))

      case Ln2(v)      => v.reverse[G2](g :/ v)
      case Exp2(v)     => v.reverse[G2](g :* Exp2(v))
      case Sqrt2(v)    => v.reverse[G2](g :* (Half2(v.shape) / Sqrt2(v)))
      case Pow02(l, r) => {
        val lhs = l.reverse[G2]((g :* r) * Pow02(l, r :- One0()))
        val rhs = r.reverse[G2]((g * Ln0(l)) :* Pow02(l, r))
        lhs ++ rhs
      }
      case Pow20(l, r) => {
        val lhs = l.reverse[G2]((g * r) :* Pow20(l, r  - One0()))
        val rhs = r.reverse[G2](g :* Ln2(l) * Pow20(l, r))
        lhs ++ rhs
      }
      case Pow22(l, r) => {
        val lhs = l.reverse[G2](g :* r * Pow22(l, r :- One0()))
        val rhs = r.reverse[G2](g :* Ln2(l) * Pow22(l, r))
        lhs ++ rhs
      }

      // Experimental
      case Abs2(v)     => Grad.where(Gt20(v, Zero0()), v.reverse[G](g), v.reverse[G](-g))
      case Max22(l, r) => Grad.where(Gt22(l, r),       l.reverse[G](g), r.reverse[G]( g))
      case Min22(l, r) => Grad.where(Lt22(l, r),       l.reverse[G](g), r.reverse[G]( g))
    }
  }

  implicit def reverse21: Reverse[V2, V1] = new Reverse[V2, V1] {

    private[this] type N  = V2
    private[this] type G  = V1
    private[this] type G2 = V2

    def reverse(n: N, g: G): Grad = n match {

      // FIXME: former g * N2 calculation foreach case for running shape check
      case _: Var2    => g :* One2(n);  Grad(n, g)
      case _: ArbVar2 => g :* One2(n);  Grad(n, g)
      case _: Zero2   => g :* Zero2(n); Grad.empty
      case _: Half2   => g :* Zero2(n); Grad.empty
      case _: One2    => g :* Zero2(n); Grad.empty
      case _: Const2  => g :* Zero2(n); Grad.empty

      case Pos2(v) => v.reverse[G](+g)
      case Neg2(v) => v.reverse[G](-g)
      case Transpose2(v) => v.reverse[G](g.T)

      case Add02(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add20(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add22(l, r) => l.reverse[G](g) ++ r.reverse[G](g)

      case Sub02(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub20(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub22(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)

      case Mul02(l, r) => l.reverse[G2](g :* r) ++ r.reverse[G](l :* g)
      case Mul20(l, r) => l.reverse[G](g :* r) ++ r.reverse[G2](l :* g)
      case Mul22(l, r) => l.reverse[G2](g :* r) ++ r.reverse[G2](l :* g)

      case Div02(l, r) => l.reverse[G2](g :/ r) ++ r.reverse[G2]((-l :* g) :/ (r * r))
      case Div20(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G2]((-l :* g) :/ (r * r))
      case Div22(l, r) => l.reverse[G2](g :/ r) ++ r.reverse[G2]((-l :* g)  / (r * r))

      case Sin2(v) => v.reverse[G2](g :* Cos2(v))
      case Cos2(v) => v.reverse[G2](-g :* Sin2(v))
      case Tan2(v) => v.reverse[G2](g :* (One2(v.shape) + (Tan2(v) * Tan2(v))))

      case Asin2(v) => v.reverse[G2](g :*  (One2(v.shape) / Sqrt2(One2(v.shape) - (v * v))))
      case Acos2(v) => v.reverse[G2](g :* -(One2(v.shape) / Sqrt2(One2(v.shape) - (v * v))))
      case Atan2(v) => v.reverse[G2](g :*  (One2(v.shape) / (One2(v.shape) + (v * v))))

      case Sinh2(v) => v.reverse[G2](g :* Cosh2(v))
      case Cosh2(v) => v.reverse[G2](g :* Sinh2(v))
      case Tanh2(v) => v.reverse[G2](g :* (One2(v.shape) - (Tanh2(v) * Tanh2(v))))

      case Ln2(v)      => v.reverse[G2](g :/ v)
      case Exp2(v)     => v.reverse[G2](g :* Exp2(v))
      case Sqrt2(v)    => v.reverse[G2](g :* (Half2(v.shape) / Sqrt2(v)))
      case Pow02(l, r) => {
        val lhs = l.reverse[G2]((g :* r) * Pow02(l, r :- One0()))
        val rhs = r.reverse[G2]((g :* Ln0(l)) :* Pow02(l, r))
        lhs ++ rhs
      }
      case Pow20(l, r) => {
        val lhs = l.reverse[G2]((g :* r) :* Pow20(l, r  - One0()))
        val rhs = r.reverse[G2](g :* Ln2(l) * Pow20(l, r))
        lhs ++ rhs
      }
      case Pow22(l, r) => {
        val lhs = l.reverse[G2](g :* r * Pow22(l, r :- One0()))
        val rhs = r.reverse[G2](g :* Ln2(l) * Pow22(l, r))
        lhs ++ rhs
      }

      // Experimental
      case Abs2(v)     => Grad.where(Gt20(v, Zero0()), v.reverse[G](g), v.reverse[G](-g))
      case Max22(l, r) => Grad.where(Gt22(l, r),       l.reverse[G](g), r.reverse[G]( g))
      case Min22(l, r) => Grad.where(Lt22(l, r),       l.reverse[G](g), r.reverse[G]( g))

      case MatMul22(l, r) if g.shape.transposed => l.reverse[G](MatMulR12(g,   r)) ++ r.reverse[G](MatMul2C1(l, g.T))
      case MatMul22(l, r)                       => l.reverse[G](MatMulR12(g.T, r)) ++ r.reverse[G](MatMul2C1(l, g))
    }
  }

  implicit def reverse22: Reverse[V2, V2] = new Reverse[V2, V2] {

    private[this] type N = V2
    private[this] type G = V2

    def reverse(n: N, g: G): Grad = n match {

      // FIXME: former g * N2 calculation foreach case for running shape check
      case _: Var2    => g * One2(n);  Grad(n, g)
      case _: ArbVar2 => g * One2(n);  Grad(n, g)
      case _: Zero2   => g * Zero2(n); Grad.empty
      case _: Half2   => g * Zero2(n); Grad.empty
      case _: One2    => g * Zero2(n); Grad.empty
      case _: Const2  => g * Zero2(n); Grad.empty

      case Pos2(v) => v.reverse[G](+g)
      case Neg2(v) => v.reverse[G](-g)
      case Transpose2(v) => v.reverse[G](g.T)

      case Add02(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add20(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add22(l, r) => l.reverse[G](g) ++ r.reverse[G](g)

      case Sub02(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub20(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub22(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)

      case Mul02(l, r) => l.reverse[G](g  * r) ++ r.reverse[G](l :* g)
      case Mul20(l, r) => l.reverse[G](g :* r) ++ r.reverse[G](l  * g)
      case Mul22(l, r) => l.reverse[G](g  * r) ++ r.reverse[G](l  * g)

      case Div02(l, r) => l.reverse[G](g  / r) ++ r.reverse[G]((-l :* g)  / (r * r))
      case Div20(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G]((-l  * g) :/ (r * r))
      case Div22(l, r) => l.reverse[G](g  / r) ++ r.reverse[G]((-l  * g)  / (r * r))

      case Sin2(v) => v.reverse[G](g * Cos2(v))
      case Cos2(v) => v.reverse[G](-g * Sin2(v))
      case Tan2(v) => v.reverse[G](g * (One2(v.shape) + (Tan2(v) * Tan2(v))))

      case Asin2(v) => v.reverse[G](g *  (One2(v.shape) / Sqrt2(One2(v.shape) - (v * v))))
      case Acos2(v) => v.reverse[G](g * -(One2(v.shape) / Sqrt2(One2(v.shape) - (v * v))))
      case Atan2(v) => v.reverse[G](g *  (One2(v.shape) / (One2(v.shape) + (v * v))))

      case Sinh2(v) => v.reverse[G](g * Cosh2(v))
      case Cosh2(v) => v.reverse[G](g * Sinh2(v))
      case Tanh2(v) => v.reverse[G](g * (One2(v.shape) - (Tanh2(v) * Tanh2(v))))

      case Ln2(v)      => v.reverse[G](g / v)
      case Exp2(v)     => v.reverse[G](g * Exp2(v))
      case Sqrt2(v)    => v.reverse[G](g * (Half2(v.shape) / Sqrt2(v)))
      case Pow02(l, r) => {
        val lhs = l.reverse[G]((g * r) * Pow02(l, r :- One0()))
        val rhs = r.reverse[G]((g :* Ln0(l)) * Pow02(l, r))
        lhs ++ rhs
      }
      case Pow20(l, r) => {
        val lhs = l.reverse[G]((g :* r) * Pow20(l, r  - One0()))
        val rhs = r.reverse[G](g * Ln2(l) * Pow20(l, r))
        lhs ++ rhs
      }
      case Pow22(l, r) => {
        val lhs = l.reverse[G](g * r * Pow22(l, r :- One0()))
        val rhs = r.reverse[G](g * Ln2(l) * Pow22(l, r))
        lhs ++ rhs
      }

      // Experimental
      case Abs2(v)     => Grad.where(Gt20(v, Zero0()), v.reverse[G](g), v.reverse[G](-g))
      case Max22(l, r) => Grad.where(Gt22(l, r),       l.reverse[G](g), r.reverse[G]( g))
      case Min22(l, r) => Grad.where(Lt22(l, r),       l.reverse[G](g), r.reverse[G]( g))

      case MatMul22(l, r) => l.reverse[G](MatMul22(g, r)) ++ r.reverse[G](MatMul22(l, g.T))
    }
  }
  */

}
*/
