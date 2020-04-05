import scala.io.StdIn
import scala.util.Random

trait Judge {
  def askForNumber(idx: Int): Int

  def checkSolution(num: String): Boolean
}

class TestJudge(b: Int) extends Judge {

  var testArr: Array[Int] = Array.fill(b) {
    Random.nextInt(2)
  }

  private var queries: Int = 0

  def applySideEffect(sideEffect: Int) = sideEffect match {
    case 0 => testArr = testArr.map(1 - _)
    case 1 => testArr = testArr.reverse
    case 2 => testArr = testArr.reverse.map(1 - _)
    case _ =>
  }

  override def askForNumber(idx: Int): Int = {
    println(s"Query ${queries+1} Asking for idx $idx in ${testArr.mkString("")}")
    queries = queries + 1
    if (queries % 10 == 1) {
      println(s"Applying side effect")
      applySideEffect(Random.nextInt(4))
      println(s"After side effect ${testArr.mkString("")}")
    }
    testArr(idx)
  }

  override def checkSolution(num: String): Boolean = {
    println(s"checking solution ${num} with real ${testArr.mkString("")}")
    testArr.mkString("") == num
  }
}

class RealJudge extends Judge {
  override def askForNumber(idx: Int): Int = {
    println(idx + 1)
    StdIn.readInt()
  }

  override def checkSolution(num: String): Boolean = {
    println(num)
    val res = StdIn.readLine()
    res == "Y"
  }
}

object ECellType extends Enumeration {
  val TWIN, COMPL, UNKNOWN = Value
}

case class Cell(value: Option[Int] = None, cellType: ECellType.Value = ECellType.UNKNOWN)

case class GuessArray(b: Int) {

  val arr: Array[Cell] = Array.fill(b) {
    Cell()
  }

  var maybeMirrors: Int = 0
  var maybeFlips: Int = 0

  def asSolution: String = arr.map(_.value.getOrElse(Random.nextInt(2))).mkString("")

  def maybeMirrorUpdate = maybeMirrors = maybeMirrors + 1

  def maybeFlipUpdate = maybeFlips = maybeFlips + 1

  def flipTwins = {
    arr.indices.filter(i => arr(i).cellType == ECellType.TWIN).map(i => {
      arr(i) = arr(i).value match {
        case Some(v) => arr(i).copy(value = Some(1 - v))
        case None => arr(i)
      }
    })
  }

  def mirrorComp = {
    Range(0, arr.length / 2).map(i => {
      if (arr(i).cellType == ECellType.COMPL) {
        val tmp = arr(i)
        arr(i) = arr(arr.length - i - 1)
        arr(arr.length - i - 1) = tmp
      }
    })
  }

  def update(idx: Int, value: Int) = {
    arr(b - idx - 1).value match {
      case Some(v) if v==value =>
        arr(idx) = arr(idx).copy(value = Some(value), cellType = ECellType.TWIN)
      case Some(v) if v!=value =>
        arr(idx) = arr(idx).copy(value = Some(value), cellType = ECellType.COMPL)
      case None => arr(idx) = arr(idx).copy(value = Some(value))
    }
    arr(idx).cellType match {
      case ECellType.TWIN => arr(b - idx - 1) = arr(idx)
      case ECellType.COMPL => arr(b - idx - 1) = arr(idx).copy(value = Some(1 - value))
      case _ =>
    }
  }
}

object Esab {

  def runTest(b: Int)(implicit judge: Judge): Boolean = {
    val guessArray: GuessArray = GuessArray(b)
    var finished: Boolean = false
    var attempts = 1
    while (!finished && attempts <= 150) {
      attempts match {
        case x if x>1 && x % 10 == 1 => {
          val anyTwinIdx = guessArray.arr.indexWhere(cell => cell.cellType == ECellType.TWIN)
          val anyCompIdx = guessArray.arr.indexWhere(cell => cell.cellType == ECellType.COMPL)

          if (anyTwinIdx >= 0) {
            val newValue = judge.askForNumber(anyTwinIdx)
            if (newValue != guessArray.arr(anyTwinIdx).value.get) {
              //value of twin changed - flip with possible mirror
              guessArray.flipTwins
            }
            //value of twin didn't change - mirror or none
            guessArray.maybeMirrorUpdate
            guessArray.update(anyTwinIdx, newValue)
            attempts = attempts + 1
            if(anyCompIdx<0){
              judge.askForNumber(anyTwinIdx)
              attempts = attempts + 1
            }
          }
          if (anyCompIdx >= 0 && attempts <=150) {
            val newValue = judge.askForNumber(anyCompIdx)
            if (newValue != guessArray.arr(anyCompIdx).value.get) {
              //comp value changed - flip or mirror
              guessArray.mirrorComp
            }
            guessArray.update(anyCompIdx, newValue)
            attempts = attempts + 1
            if(anyTwinIdx<0){
              judge.askForNumber(anyCompIdx)
              attempts = attempts + 1
            }
          }

          if(anyCompIdx<0 && anyTwinIdx<0){
            throw new Exception("shouldn't happen")
          }

        }
        case x if x % 2 == 1 => {
          val idx = guessArray.arr.indexWhere(cell => cell.cellType == ECellType.UNKNOWN)
          if(idx>=0){
            val res = judge.askForNumber(idx)
            guessArray.update(idx, res)
            attempts = attempts + 1
          }else{
            finished = true
          }
        }
        case x if x % 2 == 0 => {
          val idx = guessArray.arr.lastIndexWhere(cell => cell.cellType == ECellType.UNKNOWN)
          if(idx >= 0){
            val res = judge.askForNumber(idx)
            guessArray.update(idx, res)
            attempts = attempts + 1
          }else{
            finished = true
          }
        }
      }
    }

    judge.checkSolution(guessArray.asSolution)
  }

  def main(args: Array[String]): Unit = {
    val Array(t, b): Array[Int] = StdIn.readLine().split(" ").map(_.toInt)
    Range(1, t + 1).foreach(testIdx => {
      implicit val judge: Judge = new TestJudge(b)
      if(!runTest(b)){
        println("Failed")
        System.exit(1)
      }
    })

  }
}
