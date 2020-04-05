import scala.io.StdIn

case class LatinSquareIndicator(trace: Int, rowsRepeating: Int = 0, colsRepeating: Int = 0)

class RowRepeatingIndicator(size: Int){

  var isRepeating: Boolean = false
  val numsContained: Array[Boolean] = Array.fill(size) { false }

  def addNum(x: Int): Unit = {
    if(!isRepeating) {
      if (!numsContained(x-1)) {
        numsContained(x-1) = true
      }else{
        isRepeating = true
      }
    }
  }
}

object Vestigium {

  def calculateLatinSquareIndicator(matrix: Array[Array[Int]], N: Int): LatinSquareIndicator = {
    var trace: Int = 0
    val rowsIndicators: Array[RowRepeatingIndicator] = Array.fill(N) { new RowRepeatingIndicator(N) }
    val colsIndicators: Array[RowRepeatingIndicator] = Array.fill(N) { new RowRepeatingIndicator(N) }
    for{
      i <- Range(0, N)
      j <- Range(0, N)
    } yield {
      val matrixValue: Int = matrix(i)(j)
      if(i==j){
        trace = trace + matrixValue
      }
      rowsIndicators(i).addNum(matrixValue)
      colsIndicators(j).addNum(matrixValue)
    }

    val rowsRepeating: Int = rowsIndicators.count(_.isRepeating)
    val colsRepeating: Int = colsIndicators.count(_.isRepeating)

    LatinSquareIndicator(trace, rowsRepeating, colsRepeating)
  }

  def main(args: Array[String]): Unit = {
    val numOfTests: Int = StdIn.readLine().toInt
    val res = Range(1, numOfTests+1).map(testNum => {
      val N: Int = StdIn.readLine().toInt
      val matrix: Array[Array[Int]] = Array.ofDim(N)
      Range(0, N).foreach(i => {
        val line: String = StdIn.readLine()
        matrix(i) = line.split(" ").map(_.toInt)
      })

      val indicator = calculateLatinSquareIndicator(matrix, N)
      s"Case #${testNum}: ${indicator.trace} ${indicator.rowsRepeating} ${indicator.colsRepeating}"
    })

    res.foreach(println)

  }

}
