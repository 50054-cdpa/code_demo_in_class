package sutd.compiler

import scala.language.adhocExtensions
import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.FindMax.* 

class TestFindMax extends funsuite.AnyFunSuite {
    test("findMax(List(1,2,3)) == 3") {
        val input = List(1,2,3)
        val result = findMax(input)
        val expected = 3
        assert(expected == result)
    }
}