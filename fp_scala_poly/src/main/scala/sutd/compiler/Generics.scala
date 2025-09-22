package sutd.compiler

object Generics {
    def reverse[A](l: List[A]): List[A] = l match {
        case Nil        => Nil
        case (hd :: tl) => reverse(tl) ++ List(hd)
    }

    enum MyList[+A] {
        case Nil // type error is fixed.
        case Cons(x: A, xs: MyList[A])
    }

    def mapML[A, B](l: MyList[A])(f: A => B): MyList[B] = l match {
        case MyList.Nil          => MyList.Nil
        case MyList.Cons(hd, tl) => MyList.Cons(f(hd), mapML(tl)(f))
    }
}
