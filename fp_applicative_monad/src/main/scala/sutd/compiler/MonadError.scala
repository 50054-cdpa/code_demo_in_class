package sutd.compiler

import sutd.compiler.Applicative.*
import sutd.compiler.Monad.*

object MonadError {
    trait ApplicativeError[F[_], E] extends Applicative[F] {
        def raiseError[A](e: E): F[A]
    }

    trait MonadError[F[_], E] extends Monad[F] with ApplicativeError[F, E] {
        override def pure[A](v: A): F[A]
        override def raiseError[A](e: E): F[A]
    }

    type ErrMsg = String

    given optMonadError: MonadError[Option, ErrMsg] =
        new MonadError[Option, ErrMsg] {
            def raiseError[A](e: ErrMsg): Option[A] = None
            def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
                fa match {
                    case None    => None
                    case Some(a) => f(a)
                }
            def pure[A](v: A): Option[A] = Some(v)
        }

    def eval(e: MathExp)(using m: MonadError[Option, ErrMsg]): Option[Int] =
        e match {
            case MathExp.Plus(e1, e2) =>
                for {
                    v1 <- eval(e1)
                    v2 <- eval(e2)
                } yield (v1 + v2)
            case MathExp.Minus(e1, e2) =>
                for {
                    v1 <- eval(e1)
                    v2 <- eval(e2)
                } yield (v1 - v2)
            case MathExp.Mult(e1, e2) =>
                for {
                    v1 <- eval(e1)
                    v2 <- eval(e2)
                } yield (v1 * v2)
            case MathExp.Div(e1, e2) =>
                for {
                    v1 <- eval(e1)
                    v2 <- eval(e2)
                    _ <-
                        if (v2 != 0) {
                            m.raiseError("div by zero encountered.")
                        } else { m.pure(()) }
                } yield (v1 / v2)
            case MathExp.Const(i) => m.pure(i)
        }

    enum Either[+A, +B] {
        case Left(v: A)
        case Right(v: B)
        // to support for comprehension
        def flatMap[C>:A,D](f: B => Either[C,D]):Either[C,D] = this match {
            case Left(a) => Left(a)
            case Right(b) => f(b)
        }
        def map[D](f:B => D):Either[A,D] = this match {
            case Right(b) => Right(f(b))
            case Left(e) => Left(e)
        }
    } 

    type EitherErr = [B] =>> Either[ErrMsg, B]


    given eitherErrMonad: MonadError[EitherErr, ErrMsg] =
        new MonadError[EitherErr, ErrMsg] {
            import Either.*
            def raiseError[B](e: ErrMsg): EitherErr[B] = Left(e)
            def bind[A, B](
                fa: EitherErr[A]
            )(f: A => EitherErr[B]): EitherErr[B] = fa match {
                case Right(b) => f(b)
                case Left(s)  => Left(s)
            }
            def pure[B](v: B): EitherErr[B] = Right(v)
        }

    def eval2(
        e: MathExp
    )(using m: MonadError[EitherErr, ErrMsg]): EitherErr[Int] = e match {
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
                _ <-
                    if (v2 != 0) { m.raiseError("div by zero encountered.") }
                    else { m.pure(()) }
            } yield (v1 / v2)
        case MathExp.Const(i) => m.pure(i)
    }
}
