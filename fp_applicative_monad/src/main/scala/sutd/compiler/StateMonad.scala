package sutd.compiler

import sutd.compiler.Applicative.*
import sutd.compiler.Monad.*

object StateMonad {
    case class State[S,A]( run:S=>(S,A)) { 
        def flatMap[B](f: A => State[S,B]):State[S,B] = this match {
            case State(ssa) => State(
                s=> ssa(s) match {
                    case (s1,a) => f(a) match {
                        case State(ssb) => ssb(s1)
                    }
                }
            )
        }
        def map[B](f:A => B):State[S,B] = this match {
            case State(ssa) => State(
                s=> ssa(s) match {
                    case (s1, a) => (s1, f(a))
                }
            )
        }
    }

    type StateM = [S] =>> [A] =>> State[S,A]

    trait StateMonad[S] extends Monad[StateM[S]] {
        override def pure[A](v:A):State[S,A] = State( s=> (s,v))
        override def bind[A,B](
            fa:State[S,A]
            )(
                ff:A => State[S,B]
            ):State[S,B] = fa.flatMap(ff)
        def get:State[S, S] = State(s => (s,s))
        def set(v:S):State[S,Unit] = State(s => (v,()))
    }

    case class Counter(c:Int)

    given counterStateMonad:StateMonad[Counter] = new StateMonad[Counter]  {
    }

    def incr(using csm:StateMonad[Counter]):State[Counter,Unit] = for {
        Counter(c) <- csm.get
        _ <- csm.set(Counter(c+1))
    } yield ()

    def app(using csm:StateMonad[Counter]):State[Counter, Int] = for {
        _ <- incr
        _ <- incr
        Counter(v) <- csm.get
    } yield v


    def runApp():Unit = app match {
        case State(run) => run(Counter(0)) match {
            case (s,v) => println((s,v)) 
        }
    }
}