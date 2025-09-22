package sutd.compiler

object TypeClass {
    enum Contact {
        case Email(e: String)
        case Phone(ph: String)
    }
    case class Person(name: String, contacts: List[Contact])
    case class Team(members: List[Person])

    trait JS[A] {
        def toJS(v: A): String
    }

    given toJSInt: JS[Int] = new JS[Int] {
        def toJS(v: Int): String = v.toString
    }

    given toJSString: JS[String] = new JS[String] {
        def toJS(v: String): String = s"'${v}'"
    }

    given toJSBoolean: JS[Boolean] = new JS[Boolean] {
        def toJS(v: Boolean): String = v.toString
    }

    given toJSContact(using jsstr: JS[String]): JS[Contact] = new JS[Contact] {
        import Contact.*
        def toJS(c: Contact): String = c match {
            case Email(e) =>
                s"{'email': ${jsstr.toJS(e)}}" // copmilation error is fixed
            case Phone(ph) =>
                s"{'phone': ${jsstr.toJS(ph)}}" // compilation erro is fixed
        }
    }

    given toJSPerson(using
        jsstr: JS[String],
        jsl: JS[List[Contact]]
    ): JS[Person] = new JS[Person] {
        def toJS(p: Person): String = p match {
            case Person(name, contacts) =>
                s"{'person':{ 'name':${jsstr.toJS(name)},  'contacts':${jsl.toJS(contacts)}}}"
        }
    }

    given toJSTeam(using jsl: JS[List[Person]]): JS[Team] = new JS[Team] {
        def toJS(t: Team): String = t match {
            case Team(members) => s"{'team':{ 'members':${jsl.toJS(members)}}}"
        }
    }

    given toJSList[A](using jsa: JS[A]): JS[List[A]] = new JS[List[A]] {
        def toJS(as: List[A]): String = {
            val j = as.map(a => jsa.toJS(a)).mkString(",")
            s"[${j}]"
        }
    }

    import Contact.*
    val myTeam = Team(
      List(
        Person("kenny", List(Email("kenny_lu@sutd.edu.sg"))),
        Person("simon", List(Email("simon_perrault@sutd.edu.sg")))
      )
    )

    assert(
      toJSTeam.toJS(
        myTeam
      ) == """{'team':{ 'members':[{'person':{ 'name':'kenny',  'contacts':[{'email': 'kenny_lu@sutd.edu.sg'}]}},{'person':{ 'name':'simon',  'contacts':[{'email': 'simon_perrault@sutd.edu.sg'}]}}]}}"""
    )

    def printAsJSON[A](v: A)(using jsa: JS[A]): Unit = {
        println(jsa.toJS(v))
    }

    printAsJSON(myTeam)
}
