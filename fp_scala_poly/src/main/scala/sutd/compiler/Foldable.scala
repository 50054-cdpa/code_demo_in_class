package sutd.compiler

import sutd.compiler.BTree.*

object Foldable {
    trait Foldable[T[_]]{
        def foldLeft[A,B](t:T[B])(acc:A)(f:(A,B)=>A):A
    }

    given listFoldable:Foldable[List] = new Foldable[List] {
        def foldLeft[A,B](t:List[B])(acc:A)(f:(A,B)=>A):A = t.foldLeft(acc)(f)
    }

    given btreeFoldable:Foldable[BTree] = new Foldable[BTree] {
        import BTree.*
        def foldLeft[A,B](t:BTree[B])(acc:A)(f:(A,B)=>A):A = t match {
            case Empty => acc
            case Node(v, lft, rgt) => {
                val acc1 = f(acc,v)
                val acc2 = foldLeft(lft)(acc1)(f)
                foldLeft(rgt)(acc2)(f)
            }
        }
    }
    val l = List(1, 2, 3)
    

    val t = BTree.Node(
      2,
      BTree.Node(1, BTree.Empty, BTree.Empty),
      BTree.Node(3, BTree.Empty, BTree.Empty)
    )
    listFoldable.foldLeft(l)(0)((x:Int,y:Int) => x + y)
    btreeFoldable.foldLeft(t)(0)((x:Int,y:Int) => x + y)
}