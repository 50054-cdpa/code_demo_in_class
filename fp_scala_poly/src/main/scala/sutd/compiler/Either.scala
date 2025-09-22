package sutd.compiler

import sutd.compiler.MathExp.*
object Either {
    type ErrMsg = String

    def eval(e: MathExp): Either[ErrMsg,Int] = e match {
        case MathExp.Plus(e1, e2) =>
            eval(e1) match {
                case Left(m) => Left(m)
                case Right(v1) =>
                    eval(e2) match {
                        case Left(m) => Left(m)
                        case Right(v2) => Right(v1 + v2)
                    }
            }
        case MathExp.Minus(e1, e2) =>
            eval(e1) match {
                case Left(m) => Left(m)
                case Right(v1) =>
                    eval(e2) match {
                        case Left(m) => Left(m)
                        case Right(v2) => Right(v1 - v2)
                    }
            }
        case MathExp.Mult(e1, e2) =>
            eval(e1) match {
                case Left(m) => Left(m)
                case Right(v1) =>
                    eval(e2) match {
                        case Left(m) => Left(m)
                        case Right(v2) => Right(v1 * v2)
                    }
            }
        case MathExp.Div(e1, e2) =>
            eval(e1) match {
                case Left(m) => Left(m)
                case Right(v1) =>
                    eval(e2) match {
                        case Left(m) => Left(m)
                        case Right(0) =>
                            Left(s"div by zero caused by ${e.toString}")
                        case Right(v2) => Right(v1 / v2)
                    }
            }
        case MathExp.Const(i) => Right(i)
    }

}
