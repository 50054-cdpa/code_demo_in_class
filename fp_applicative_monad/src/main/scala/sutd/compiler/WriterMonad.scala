package sutd.compiler

import sutd.compiler.Applicative.*
import sutd.compiler.Monad.*

object WriterMonad {
    trait Monoid[A] { // We omitted the super class SemiRing[A]
        def mempty: A
        def mappend: A => A => A
    }

    given listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
        def mempty: List[A] = Nil
        def mappend: List[A] => List[A] => List[A] =
            (l1: List[A]) => (l2: List[A]) => l1 ++ l2
    }

    case class Writer[W, A](run: (W, A))(using mw: Monoid[W]) {
        def flatMap[B](f: A => Writer[W, B]): Writer[W, B] = this match {
            case Writer((w, a)) =>
                f(a) match {
                    case Writer((w2, b)) => Writer((mw.mappend(w)(w2), b))
                }
        }
        def map[B](f: A => B): Writer[W, B] = this match {
            case Writer((w, a)) => Writer((w, f(a)))
        }
    }

    type WriterM = [W] =>> [A] =>> Writer[W, A]

    trait WriterMonad[W] extends Monad[WriterM[W]] {
        implicit def W0:Monoid[W]
        override def pure[A](v: A): Writer[W, A] = Writer((W0.mempty, v))
        override def bind[A, B](
            fa: Writer[W, A]
        )(f: A => Writer[W, B]): Writer[W, B] = fa.flatMap(f)
        def tell(w: W): Writer[W, Unit] = Writer((w, ()))
        def pass[A](ma: Writer[W, (A, W => W)]): Writer[W, A] = ma match {
            case Writer((w, (a, f))) => Writer((f(w), a))
        }
    }


    case class LogEntry(msg: String)

    given logWriterMonad:WriterMonad[List[LogEntry]] = new WriterMonad[List[LogEntry]] {
        override def W0 = new Monoid[List[LogEntry]] {
            override def mempty = Nil
            override def mappend = (x:List[LogEntry]) => (y:List[LogEntry]) => x ++ y

        }
    }

    def logger(m: String)(using
        wm: WriterMonad[List[LogEntry]]
    ): Writer[List[LogEntry], Unit] = wm.tell(List(LogEntry(m)))

    def app(using
        wm: WriterMonad[List[LogEntry]]
    ): Writer[List[LogEntry], Int] = for {
        _ <- logger("start")
        x <- wm.pure(1 + 1)
        _ <- logger(s"result is ${x}")
        _ <- logger("done")
    } yield x

    def runApp(): Int = app match {
        case Writer((w, i)) => {
            println(w)
            i
        }
    }
}
