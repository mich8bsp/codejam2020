import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object EPerson extends Enumeration {
  val J, C = Value
}

case class Activity(id: Int, start: Int, end: Int, assignedTo: Option[EPerson.Value] = None)

object ParentPartnerRet {

  def assignActivities(activities: List[Activity], jEnding: Int = 0, cEnding: Int = 0, res: mutable.Buffer[Activity] = mutable.Buffer()): List[Activity] = activities match {
    case Nil => res.toList
    case x::xs => {
      if (x.start >= jEnding) {
        res.append(x.copy(assignedTo = Some(EPerson.J)))
        assignActivities(xs, x.end, cEnding, res)
      } else if (x.start >= cEnding) {
        res.append(x.copy(assignedTo = Some(EPerson.C)))
        assignActivities(xs, jEnding, x.end, res)
      } else {
        throw new Exception()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val T: Int = StdIn.readLine().toInt
    val res = Range(1, T + 1).map(caseNum => {
      val N = StdIn.readLine().toInt

      val activities: List[Activity] = Range(1, N+1).map(i => {
        val Array(start, end) = StdIn.readLine().split(" ")
        Activity(i, start.toInt, end.toInt)
      }).toList

      val assignment: Try[List[Activity]] = Try(assignActivities(activities.sortBy(_.start)))

      assignment match {
        case Success(v) => {
          val sorted = v.sortBy(_.id)
          val resAss: String = sorted.flatMap(_.assignedTo).map(_.toString).mkString("")
          s"Case #$caseNum: $resAss"
        }

        case Failure(e) => {
          s"Case #$caseNum: IMPOSSIBLE"
        }
      }

    })

    res.foreach(println)
  }
}
