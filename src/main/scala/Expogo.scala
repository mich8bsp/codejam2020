import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}


object Expogo {

  object Direction extends Enumeration {
    val N, W, E, S = Value

    def toDelta(d: Direction.Value): (Int, Int) = d match {
      case N => (0, 1)
      case W => (-1, 0)
      case E => (1, 0)
      case S => (0, -1)
    }
  }

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")

  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)

  def readInputStr: String = StdIn.readLine()

  def readInputInt: Int = StdIn.readInt()
//
//  def makeMoveTo(finalX: Int, finalY: Int, currX: Int, currY: Int, currMove: Int = 0, movesSoFar: List[Direction.Value], direction: Direction.Value) = {
//    val (deltaX, deltaY) = Direction.toDelta(direction)
//    val stepSize = math.pow(2, currMove)
//    findDirectionsTo(finalX, finalY, currX + deltaX * stepSize.toInt, currY + deltaY * stepSize.toInt, currMove + 1, movesSoFar.:+(direction))
//  }

  def isImpossible(finalX: Int, finalY: Int, currX: Int, currY: Int, currMove: Int) = {
    val step = math.pow(2, currMove)

    val res = step > math.abs(finalX - currX) && step > math.abs(finalY - currY)
    res
  }

  def findDirectionsTo(finalX: Int, finalY: Int, currX: Int, currY: Int, currMove: Int = 0, movesSoFar: mutable.Buffer[Direction.Value] = mutable.Buffer(), bestSoFar: Option[Int] = None): List[Direction.Value] = {
    if ((currX, currY) == (finalX, finalY)) {
      movesSoFar.toList
    } else {
      if(bestSoFar.exists(_ <= currMove) || isImpossible(finalX, finalY, currX, currY, currMove)){
        throw new Exception()
      }
      var bestSoFarInCurr: Option[Int] = None
      val movedN: Try[List[Direction.Value]] = Try {
        val direction = Direction.N
        val (deltaX, deltaY) = Direction.toDelta(direction)
        val stepSize = math.pow(2, currMove)
        movesSoFar.append(direction)
        findDirectionsTo(finalX, finalY, currX + deltaX * stepSize.toInt, currY + deltaY * stepSize.toInt, currMove + 1, movesSoFar)
      }
      movesSoFar.remove(movesSoFar.length-1)
      if(movedN.isSuccess){
        bestSoFarInCurr = Some(movedN.get.length)
      }
      val movedW: Try[List[Direction.Value]] = Try {
        val direction = Direction.W
        val (deltaX, deltaY) = Direction.toDelta(direction)
        val stepSize = math.pow(2, currMove)
        movesSoFar.append(direction)
        findDirectionsTo(finalX, finalY, currX + deltaX * stepSize.toInt, currY + deltaY * stepSize.toInt, currMove + 1, movesSoFar , bestSoFarInCurr)
      }
      movesSoFar.remove(movesSoFar.length-1)

      if(movedW.isSuccess){
        bestSoFarInCurr = Some(math.min(movedW.get.length, bestSoFarInCurr.getOrElse(Int.MaxValue)))
      }

      val movedS: Try[List[Direction.Value]] =  Try {
        val direction = Direction.S
        val (deltaX, deltaY) = Direction.toDelta(direction)
        val stepSize = math.pow(2, currMove)
        movesSoFar.append(direction)
        findDirectionsTo(finalX, finalY, currX + deltaX * stepSize.toInt, currY + deltaY * stepSize.toInt, currMove + 1, movesSoFar , bestSoFarInCurr)
      }
      movesSoFar.remove(movesSoFar.length-1)

      if(movedS.isSuccess){
        bestSoFarInCurr = Some(math.min(movedS.get.length, bestSoFarInCurr.getOrElse(Int.MaxValue)))
      }

      val movedE: Try[List[Direction.Value]] =  Try {
        val direction = Direction.E
        val (deltaX, deltaY) = Direction.toDelta(direction)
        val stepSize = math.pow(2, currMove)
        movesSoFar.append(direction)
        findDirectionsTo(finalX, finalY, currX + deltaX * stepSize.toInt, currY + deltaY * stepSize.toInt, currMove + 1, movesSoFar , bestSoFarInCurr)
      }
      movesSoFar.remove(movesSoFar.length-1)

      List(movedN, movedW, movedS, movedE)
        .flatMap(_.toOption)
        .minBy(_.length)
    }
  }

  def runTestCase: Try[String] = {
    val Array(x, y) = readInputArrInt
    Try(findDirectionsTo(x, y, 0, 0))
      .map(_.map(_.toString).mkString(""))
  }

  def main(args: Array[String]): Unit = {
    val T: Int = readInputInt
    val totalRes = Range(1, T + 1).flatMap(testIdx => {

      val testCaseRes: Try[String] = runTestCase
      testCaseRes match {
        case Success(v) => List(s"Case #$testIdx: $v")
        case Failure(exception) => List(s"Case #$testIdx: IMPOSSIBLE")
      }
    })

    totalRes.foreach(println)
  }

}
