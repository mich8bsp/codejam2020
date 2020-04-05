import scala.annotation.tailrec
import scala.io.StdIn

object NestingDepth {

  @tailrec
  def buildRes(numsLeft: List[Int], parenOpen: Int = 0, res: String = ""): String = numsLeft match {
    case Nil => res + (")" * parenOpen)
    case x::xs => {
      val parenOfChoice = if(parenOpen > x) ")" else "("
      val newRes = res + parenOfChoice * math.abs(parenOpen - x) + x.toString
      buildRes(xs, x, newRes)
    }
  }

  def main(args: Array[String]): Unit = {
    val T: Int = StdIn.readLine().toInt
    val res = Range(1, T + 1).map(caseNum => {
      val S = StdIn.readLine()
      val Stag = buildRes(S.map(_.asDigit).toList)
      s"Case #$caseNum: $Stag"
    })
    res.foreach(println)
  }
}