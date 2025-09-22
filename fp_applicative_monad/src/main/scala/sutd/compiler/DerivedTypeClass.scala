package sutd.compiler

object DerivedTypeClass {
    trait Eq[A] {
        def eqv(x: A, y: A): Boolean
    }

    trait Order[A] extends Eq[A] {
        def compare(x: A, y: A): Int
        def eqv(x: A, y: A): Boolean = compare(x, y) == 0
        def gt(x: A, y: A): Boolean = compare(x, y) > 0
        def lt(x: A, y: A): Boolean = compare(x, y) < 0
    }

    given eqInt: Eq[Int] = new Eq[Int] {
        def eqv(x: Int, y: Int): Boolean = x == y
    }

    given orderInt: Order[Int] = new Order[Int] {
        def compare(x: Int, y: Int): Int = x - y
    }

    def main() = {
        assert(eqInt.eqv(1, 1))
        assert(orderInt.eqv(1, 1))
    }


    // Alternative

    /*
    trait Order[A] extends Eq[A] { 
        def compare(x:A, y:A):Int
        // def eqv(x:A, y:A):Boolean = compare(x,y) == 0
        def gt(x:A,  y:A):Boolean = compare(x,y) > 0
        def lt(x:A,  y:A):Boolean = compare(x,y) < 0
    }

    given eqInt:Eq[Int] = new Eq[Int] {
        def eqv(x:Int, y:Int):Boolean = x == y
    }

    given orderInt(using eqInt:Eq[Int]):Order[Int] = new Order[Int] {
        def eqv(x:Int,y:Int):Boolean = eqInt.eqv(x,y)
        def compare(x:Int, y:Int):Int = x - y
    }

    eqInt.eqv(1,1)
    orderInt.eqv(1,1)
    */
}
