package sutd.compiler
import sutd.compiler.Applicative.*
import sutd.compiler.Monad.*

object MyState {
    case class MyState[S, A](run: S => Option[(S, A)]) {
        def flatMap[B](f: A => MyState[S, B]): MyState[S, B] = this match {
            case MyState(ssa) =>
                MyState(s =>
                    ssa(s) match {
                        case None => None
                        case Some((s1, a)) =>
                            f(a) match {
                                case MyState(ssb) => ssb(s1)
                            }
                    }
                )
        }
        def map[B](f: A => B): MyState[S, B] = this match {
            case MyState(ssa) =>
                MyState(s =>
                    ssa(s) match {
                        case None          => None
                        case Some((s1, a)) => Some((s1, f(a)))
                    }
                )
        }
    }

    type MyStateM = [S] =>> [A] =>> MyState[S,A]

    trait MyStateMonad[S] extends Monad[MyStateM[S]] {
        override def pure[A](v:A):MyState[S,A] = MyState( s=> Some((s,v)))
        override def bind[A,B](
            fa:MyState[S,A]
            )(
                ff:A => MyState[S,B]
            ):MyState[S,B] = fa.flatMap(ff)
        def get:MyState[S, S] = MyState(s => Some((s,s)))
        def set(v:S):MyState[S,Unit] = MyState(s => Some((v,())))
    }

}
