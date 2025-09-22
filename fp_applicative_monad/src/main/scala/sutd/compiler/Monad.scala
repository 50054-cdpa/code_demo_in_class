package sutd.compiler

import sutd.compiler.Applicative.*

object Monad {

    enum MathExp {
        case Plus(e1: MathExp, e2: MathExp)
        case Minus(e1: MathExp, e2: MathExp)
        case Mult(e1: MathExp, e2: MathExp)
        case Div(e1: MathExp, e2: MathExp)
        case Const(v: Int)
    }
    
    trait Monad[F[_]] extends Applicative[F] {
        def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
        def pure[A](v: A): F[A]
        def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
            bind(ff)((f: A => B) => bind(fa)((a: A) => pure(f(a))))
    }

    given optMonad: Monad[Option] = new Monad[Option] {
        def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
            case None    => None
            case Some(a) => f(a)
        }
        def pure[A](v: A): Option[A] = Some(v)
    }
    def eval(e: MathExp)(using m: Monad[Option]): Option[Int] = e match {
        case MathExp.Plus(e1, e2) =>
            m.bind(eval(e1))(v1 => {
                m.bind(eval(e2))({ v2 => m.pure(v1 + v2) })
            })
        case MathExp.Minus(e1, e2) =>
            m.bind(eval(e1))(v1 => {
                m.bind(eval(e2))({ v2 => m.pure(v1 - v2) })
            })
        case MathExp.Mult(e1, e2) =>
            m.bind(eval(e1))(v1 => {
                m.bind(eval(e2))({ v2 => m.pure(v1 * v2) })
            })
        case MathExp.Div(e1, e2) =>
            m.bind(eval(e1))(v1 => {
                m.bind(eval(e2))({ v2 =>
                    if (v2 == 0) { None }
                    else { m.pure(v1 / v2) }
                })
            })
        case MathExp.Const(i) => m.pure(i)
    }

    /* support for ... yield */

    def eval2(e: MathExp)(using m: Monad[Option]): Option[Int] = e match {
        case MathExp.Plus(e1, e2) =>
            for {
                v1 <- eval2(e1)
                v2 <- eval2(e2)
            } yield (v1 + v2)
        case MathExp.Minus(e1, e2) =>
            for {
                v1 <- eval2(e1)
                v2 <- eval2(e2)
            } yield (v1 - v2)
        case MathExp.Mult(e1, e2) =>
            for {
                v1 <- eval2(e1)
                v2 <- eval2(e2)
            } yield (v1 * v2)
        case MathExp.Div(e1, e2) =>
            for {
                v1 <- eval2(e1)
                v2 <- eval2(e2)
                if (v2 != 0)
            } yield (v1 / v2)
        case MathExp.Const(i) => m.pure(i)
    }

    import MathExp.*
    def test():Option[Int] =
        eval(Div(Const(1), Minus(Const(2), Const(2))))

}
