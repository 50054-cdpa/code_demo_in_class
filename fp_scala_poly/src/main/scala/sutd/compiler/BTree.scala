package sutd.compiler

object BTree {
    enum BTree[+A] {
        case Empty
        case Node(v: A, lft: BTree[A], rgt: BTree[A])
    }

}