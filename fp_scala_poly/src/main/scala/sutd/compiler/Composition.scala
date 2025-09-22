package sutd.compiler

object Composition {
    def f(x: Int): Int = 2 * x + 3
    def g(x: Int): Int = x * x

    assert((g.compose(f))(2) == g(f(2)))
}
