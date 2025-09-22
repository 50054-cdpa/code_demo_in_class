package sutd.compiler

object FindMax {
    def findMax(l:List[Int]):Int = l match {
        case Nil => Int.MinValue
        case (hd::tl) => {
            val max_tl = findMax(tl)
            if (hd > max_tl) {
                hd 
            } else {
                max_tl
            }
        }
    }
}