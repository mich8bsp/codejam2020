import scala.io.StdIn

object Solution {

  def readInputArrStr: Array[String] = StdIn.readLine().split(" ")
  def readInputArrInt: Array[Int] = readInputArrStr.map(_.toInt)
  def readInputStr: String = StdIn.readLine()
  def readInputInt: Int = StdIn.readInt()

  def chooseTarget(a: Int) = (0, 0)

  def runTestCase(a: Int, b: Int): Unit = {

    var isSuccess: Boolean = false
    var attempts: Int = 0
    val distance: Int = (math.pow(10, 9) - a).toInt
    val targets = for{
      i <- Range(-distance, distance)
      j <- Range(-distance, distance)
    }yield{
      (i, j)
    }
    var currIdx = 0
    while(!isSuccess && attempts<300){
      val (x, y) = targets(currIdx)
      currIdx += 1
      println(s"$x $y")
      val res = readInputStr
      res match {
        case "CENTER" => isSuccess = true
        case "HIT" =>
        case "MISS" =>
      }
      attempts += 1
    }
  }

  def main(args: Array[String]): Unit = {
    val Array(t, a, b): Array[Int] = readInputArrInt
   Range(1, t + 1).foreach(testIdx => {
     runTestCase(a, b)
    })

  }

}
