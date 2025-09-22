package sutd.compiler

object Functor {
    trait Functor[T[_]] {
        def map[A, B](t: T[A])(f: A => B): T[B]
    }

    given listFunctor: Functor[List] = new Functor[List] {
        def map[A, B](t: List[A])(f: A => B): List[B] = t.map(f)
    }

    enum BTree[+A] {
        case Empty
        case Node(v: A, lft: BTree[A], rgt: BTree[A])
    }

    given btreeFunctor: Functor[BTree] = new Functor[BTree] {
        import BTree.*
        def map[A, B](t: BTree[A])(f: A => B): BTree[B] = t match {
            case Empty             => Empty
            case Node(v, lft, rgt) => Node(f(v), map(lft)(f), map(rgt)(f))
        }
    }

    val l = List(1, 2, 3)
    listFunctor.map(l)((x: Int) => x + 1)

    val t = BTree.Node(
      2,
      BTree.Node(1, BTree.Empty, BTree.Empty),
      BTree.Node(3, BTree.Empty, BTree.Empty)
    )
    btreeFunctor.map(t)((x: Int) => x + 1)
}
