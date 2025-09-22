package sutd.compiler

import sutd.compiler.Applicative.*
import sutd.compiler.Monad.*

object ReaderMonad {
    case class Reader[R, A](run: R => A) {
        // we need flatMap and map for for-comprehension
        def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = this match {
            case Reader(ra) =>
                Reader { r =>
                    f(ra(r)) match {
                        case Reader(rb) => rb(r)
                    }
                }
        }
        def map[B](f: A => B): Reader[R, B] = this match {
            case Reader(ra) =>
                Reader { r =>
                    f(ra(r))
                }
        }
    }

    type ReaderM = [R] =>> [A] =>> Reader[R, A]

    trait ReaderMonad[R] extends Monad[ReaderM[R]] {
        override def pure[A](v: A): Reader[R, A] = Reader(r => v)
        override def bind[A, B](
            fa: Reader[R, A]
        )(f: A => Reader[R, B]): Reader[R, B] = fa.flatMap(f)
        def ask: Reader[R, R] = Reader(r => r)
        def local[A](f: R => R)(r: Reader[R, A]): Reader[R, A] = r match {
            case Reader(ra) =>
                Reader(r => {
                    val localR = f(r)
                    ra(localR)
                })
        }
    }

    case class API(url: String)

    given APIReader: ReaderMonad[API] = new ReaderMonad[API] {}

    def get(path: String)(using pr: ReaderMonad[API]): Reader[API, Unit] = for {
        r <- pr.ask
        s <- r match {
            case API(url) => pr.pure(println(s"${url}${path}"))
        }
    } yield s

    def authServer(api: API): API = API("https://127.0.0.10/")

    def test1(using pr: ReaderMonad[API]): Reader[API, Unit] = for {
        a <- pr.local(authServer)(get("auth"))
        t <- get("time")
        j <- get("job")
    } yield (())

    def runtest1(): Unit = test1 match {
        case Reader(run) => run(API("https://127.0.0.1/"))
    }

}
