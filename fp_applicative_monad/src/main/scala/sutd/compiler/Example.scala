package sutd.compiler

import sutd.compiler.Functor.*
import sutd.compiler.Applicative.*

object Example {
    type EitherStr = [C] =>> Either[String, C]
    given Functor[EitherStr] {
        def map[A,B](t:EitherStr[A]) (f : A => B) : EitherStr[B] = t match {
            case Left(msg) => Left(msg) 
            case Right(a)  => Right(f(a))
        }
    }

    given Applicative[EitherStr] {
        def pure[A](a:A):EitherStr[A] = Right(a) 
        def ap[A, B](ff: EitherStr[A => B])(fa: EitherStr[A]):EitherStr[B] = ff match {
            case Left(msg) => Left(msg)
            case Right(f) => fa match {
                case Left(msg) => Left(msg)
                case Right(v) => Right(f(v))
            }
        }
    }
    val em1 = given_Applicative_EitherStr.pure(1) // EitherStr[Int]
    val emf = given_Applicative_EitherStr.pure( (x : Int)  => x + 1 ) // EitherStr[Int => Int]
    given_Applicative_EitherStr.ap(emf)(em1) // yields Right(2)
}