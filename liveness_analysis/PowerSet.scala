object PowerSet {
    def pset[A](xs:List[A]):List[List[A]] = xs match {
        case Nil => List(Nil)
        case (x::xs) => 
            val ps = pset(xs)
            ps ++ (ps.map( s => x::s ))
    }

    def hasse[A](xs:List[A]):List[(List[A],List[A])] = {
        val ps = pset(xs).sortBy( _.length )
        def isup(l1:List[A], l2:List[A]) : Boolean = {
            val s1 = l1.toSet
            val s2 = l2.toSet
            (s1.size == s2.size + 1) && s2.subsetOf(s1)
        }
        def pair(s:List[A], ps:List[List[A]]):List[(List[A],List[A])] = {
            ps.filter( t => isup(t, s) ).map( x => (x,s)) 
        }
        def go(ps:List[List[A]]):List[(List[A],List[A])] = ps match {
            case Nil => Nil
            case (q::qs) => pair(q, qs) ++ go(qs)
        }
        go(ps)
    }

    def hasse_mermaid[A](xs:List[A]):String = {
        val h = hasse(xs)
        val ps = pset(xs).sortBy( 0 - _.length )
        val m = ps.zip(1 to ps.length).toMap
        h.foldLeft("")((acc, p) => p match {
            case (parent, child) => s"${acc} \n ${node(parent,m)} --- ${node(child,m)}"  
        })
    }

    def node[A](l:List[A], m:Map[List[A],Int]):String = m.get(l) match {
        case None => "error"
        case Some(id) => {
            val lstr = l.map(_.toString).mkString(",")
            s"N${id}[\"{${lstr}}\"]"
        }
    }
}