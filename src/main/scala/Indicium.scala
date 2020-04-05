import scala.io.StdIn
import scala.util.{Failure, Random, Success, Try}

object TestPerf {
  def main(args: Array[String]): Unit = {
    val t = 100

    Range(1, t + 1).foreach(testIdx => {
      val start = System.currentTimeMillis()
      val n = Random.between(2, 8)
      val k = Random.between(n, n * n + 1)
      println(s"start solving for $n $k")
      val solution = Try(Indicium.buildNaturalLatinSquare(n, k, Array.ofDim(n, n)))
      val took = (System.currentTimeMillis() - start) / 1E3
      println(s"Finished solving for $n $k ${if (solution.isSuccess) "success" else "failure"}")
      println(s"took $took sec")
      if (took > 20) {
        println("TLE!!!!")
      }
    })
  }
}

object Indicium {

  def buildPermutations(n: Int, array: Array[Array[Int]], currRow: Int, currCol: Int, leftToUse: Set[Int], currentPerm: Array[Int]): Iterator[Array[Int]] = {
    if (leftToUse.isEmpty) {
      if (currentPerm.length == n) Iterator(currentPerm.clone()) else Iterator.empty
    } else {
      val prevRows: Set[Int] = Range(0, currRow).map(i => array(i)(currCol)).toSet
      val actualLeftToUse: Set[Int] = leftToUse -- prevRows

      actualLeftToUse.iterator.flatMap(y => {
        currentPerm(currCol) = y
        val leftToUseForNextIter: Set[Int] = leftToUse.filterNot(_ == y)
        buildPermutations(n, array, currRow, currCol + 1, leftToUseForNextIter, currentPerm)
      })
    }

  }

  def buildNaturalLatinSquare(n: Int, trace: Int, currMatrix: Array[Array[Int]], currRow: Int = 0): Array[Array[Int]] = {
    if (trace < 0 || (currRow == n && trace > 0)) {
      throw new Exception()
    }
    if (trace == 0 && currRow == n) {
      currMatrix
    } else {
      val validPermutations = buildPermutations(n, currMatrix, currRow, 0, Range(1, n + 1).toSet, Array.ofDim(n))
      val resultIterator = validPermutations.flatMap(permutation => {
        println(s"checking permutation ${permutation.mkString(" ")} for row $currRow")
        currMatrix(currRow) = permutation
        Try(buildNaturalLatinSquare(n, trace - currMatrix(currRow)(currRow), currMatrix, currRow + 1)).toOption
      })

      if (resultIterator.hasNext) {
        resultIterator.next()
      } else {
        throw new Exception()
      }
    }

  }

  def main(args: Array[String]): Unit = {
    val T: Int = StdIn.readInt()
    val res = Range(1, T + 1).flatMap(testIdx => {
      val Array(n, k): Array[Int] = StdIn.readLine().split(" ").map(_.toInt)

      val naturalLatin: Try[Array[Array[Int]]] = Try(buildNaturalLatinSquare(n, k, Array.ofDim(n, n)))

      naturalLatin match {
        case Success(v) => List(s"Case #$testIdx: POSSIBLE") ++ v.map(_.mkString(" "))
        case Failure(exception) => List(s"Case #$testIdx: IMPOSSIBLE")
      }
    })

    res.foreach(println)

  }
}
