package sutd.compiler

object MathExp {
    enum MathExp {
        case Plus(e1: MathExp, e2: MathExp)
        case Minus(e1: MathExp, e2: MathExp)
        case Mult(e1: MathExp, e2: MathExp)
        case Div(e1: MathExp, e2: MathExp)
        case Const(v: Int)
    }
}
