package sutd.compiler

import sutd.compiler.MathExp.*

object Option {

    def eval(e: MathExp): Option[Int] = e match {
        case MathExp.Plus(e1, e2) =>
            eval(e1) match {
                case None => None
                case Some(v1) =>
                    eval(e2) match {
                        case None     => None
                        case Some(v2) => Some(v1 + v2)
                    }
            }
        case MathExp.Minus(e1, e2) =>
            eval(e1) match {
                case None => None
                case Some(v1) =>
                    eval(e2) match {
                        case None     => None
                        case Some(v2) => Some(v1 - v2)
                    }
            }
        case MathExp.Mult(e1, e2) =>
            eval(e1) match {
                case None => None
                case Some(v1) =>
                    eval(e2) match {
                        case None     => None
                        case Some(v2) => Some(v1 * v2)
                    }
            }
        case MathExp.Div(e1, e2) =>
            eval(e1) match {
                case None => None
                case Some(v1) =>
                    eval(e2) match {
                        case None     => None
                        case Some(0)  => None
                        case Some(v2) => Some(v1 / v2)
                    }
            }
        case MathExp.Const(i) => Some(i)
    }
    import MathExp.*
    assert(eval(Div(Const(1), Minus(Const(2), Const(2)))) == None)
}
