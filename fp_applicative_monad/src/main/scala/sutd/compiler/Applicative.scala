package sutd.compiler

import sutd.compiler.Functor.*

object Applicative {
    trait Applicative[F[_]] extends Functor[F] {
        def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
        def pure[A](a: A): F[A]
        def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
    }

    given listApplicative: Applicative[List] = new Applicative[List] {
        def pure[A](a: A): List[A] = List(a)
        def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
            ff.map(f => fa.map(f)).flatten
    }

    listApplicative.map(l)((x:Int) => x + 1)

    val intOps= List((x:Int)=>x+1, (y:Int)=>y*2)
    listApplicative.ap(intOps)(l) == List(2, 3, 4, 2, 4, 6)
}
