package sutd.compiler

object Currying {
    def sum(x:Int, y:Int):Int = x + y
    def sum_curry(x:Int)(y:Int):Int = x + y

    def main() = 
        assert(sum(1,2) == sum_curry(1)(2))
    
    def plus1(x:Int):Int = sum_curry(1)(x)
}