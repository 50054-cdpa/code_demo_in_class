package sutd.compiler

import sutd.compiler.Monad.*
import java.util.Date
import java.util.Calendar
import java.util.GregorianCalendar
import java.text.SimpleDateFormat

object ListMonad {
    given listMonad: Monad[List] = new Monad[List] {
        def pure[A](v: A): List[A] = List(v)
        def bind[A, B](fa: List[A])(f: A => List[B]): List[B] =
            fa.flatMap(f)
    }

    case class Staff(id: Int, dob: Date)

    def mkStaff(id: Int, dobStr: String): Staff = {
        val sdf = new SimpleDateFormat("yyyy-MM-dd")
        val dobDate = sdf.parse(dobStr)
        Staff(id, dobDate)
    }
    val staffData = List(
      mkStaff(1, "1076-01-02"),
      mkStaff(2, "1986-07-24")
    )

    def ageBelow(staff: Staff, age: Int): Boolean = staff match {
        case Staff(id, dob) => {
            val today = new Date()
            val calendar = new GregorianCalendar();
            calendar.setTime(today)
            calendar.add(Calendar.YEAR, -age)
            val ageYearsAgo = calendar.getTime()
            dob.after(ageYearsAgo)
        }
    }

    def query(data: List[Staff]): List[Staff] = for {
        staff <- data // from data
        if ageBelow(staff, 40) // where staff.age < 40
    } yield staff // select *
}
