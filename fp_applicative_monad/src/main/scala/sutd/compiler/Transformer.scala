package sutd.compiler

import sutd.compiler.Applicative.*
import sutd.compiler.Monad.*

object Transformer {

    case class StateT[S, M[_], A](run: S => M[(S, A)])(using m:Monad[M]) {
        def flatMap[B](f: A => StateT[S, M, B]): StateT[S, M, B] = this match {
            case StateT(ssa) =>
                StateT(s => m.bind(ssa(s))
                    (sa => sa match {
                        case (s1,a) => f(a) match {
                            case StateT(ssb) => ssb(s1)
                            }
                        }
                    )
                ) 
            }
        
        def map[B](f: A => B): StateT[S, M, B] = this match {
            case StateT(ssa) =>
                StateT(s => m.bind(ssa(s))
                    (sa => sa match {
                        case (s1, a) => m.pure((s1, f(a)))
                    })
                )
        }
    }

    type StateTM = [S] =>> [M[_]] =>> [A] =>> StateT[S, M, A]

    trait StateTMonad[S,M[_]] extends Monad[StateTM[S][M]]  {
        implicit def M0:Monad[M]
        override def pure[A](v: A): StateT[S, M, A] = StateT(s => M0.pure((s, v)))
        override def bind[A, B](
            fa: StateT[S, M, A]
        )(
            ff: A => StateT[S, M, B]
        ): StateT[S, M, B] = fa.flatMap(ff)
        def get: StateT[S, M, S] = StateT(s => M0.pure((s, s)))
        def set(v: S): StateT[S, M, Unit] = StateT(s => M0.pure(v, ()))
    }


    case class Identity[A](run:A) {
        def flatMap[B](f:A=>Identity[B]):Identity[B] = this match {
            case Identity(a) => f(a)
        }
        def map[B](f:A=>B):Identity[B] = this match {
            case Identity(a) => Identity(f(a))
        }
    }

    given identityMonad:Monad[Identity] = new Monad[Identity] {
        override def pure[A](v:A):Identity[A] = Identity(v)
        override def bind[A,B](fa:Identity[A])(f: A => Identity[B]):Identity[B] = fa.flatMap(f)
    }

    trait StateIdentMonad[S] extends StateTMonad[S, Identity] { // same as StateMonad
        override def M0 = identityMonad
    }

    trait StateOptMonad[S] extends StateTMonad[S, Option] { 
        override def M0 = sutd.compiler.Monad.optMonad
    }


    case class ReaderT[R, M[_], A](run: R => M[A])(using m:Monad[M]) {
        def flatMap[B](f: A => ReaderT[R, M, B]):ReaderT[R, M, B] = this match {
            case ReaderT(ra) => ReaderT( r => m.bind(ra(r))
                ( a => f(a) match {
                case ReaderT(rb) => rb(r)
                }))
        }
        def map[B](f: A => B):ReaderT[R, M, B] = this match {
            case ReaderT(ra) => ReaderT( r => m.bind(ra(r))
                ( a => m.pure(f(a))))
        }
    }


    type ReaderTM = [R] =>>[M[_]] =>> [A] =>> ReaderT[R, M, A]

    trait ReaderTMonad[R,M[_]] extends Monad[ReaderTM[R][M]] {
        implicit def M0:Monad[M]
        override def pure[A](v: A): ReaderT[R, M, A] = ReaderT(r => M0.pure(v))
        override def bind[A, B](
            fa: ReaderT[R, M, A]
        )(f: A => ReaderT[R, M, B]): ReaderT[R, M, B] = fa.flatMap(f)
        def ask: ReaderT[R, M, R] = ReaderT(r => M0.pure(r))
        def local[A](f: R => R)(r: ReaderT[R, M, A]): ReaderT[R, M, A] = r match {
            case ReaderT(ra) =>
                ReaderT(r => {
                    val localR = f(r)
                    ra(localR)
                })
        }
    }
    trait ReaderIdentMonad[R] extends ReaderTMonad[R, Identity] { // same as ReaderMonad
        override def M0 = identityMonad
    }

    trait ReaderStateIdentMonad[R, S] extends ReaderTMonad[R, StateTM[S][Identity]] {
        override def M0:StateIdentMonad[S] = new StateIdentMonad[S]{}
    }

    trait StateReaderIdentMonad[S, R] extends StateTMonad[S, ReaderTM[R][Identity]] {
        override def M0:ReaderIdentMonad[R] = new ReaderIdentMonad[R]{}
    }
}
