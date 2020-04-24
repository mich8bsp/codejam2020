import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Random, Success, Try}

object TestPerf2 {
  def main(args: Array[String]): Unit = {
    val t = 100

    Range(1, t + 1).foreach(testIdx => {
      val start = System.currentTimeMillis()
      val n = Random.between(2, 10)
      val k = Random.between(n, n * n + 1)
      println(s"start solving for $n $k")
      val solution = Try(IndiciumTry2.solve(n, k))
      val took = (System.currentTimeMillis() - start) / 1E3
      println(s"Finished solving for $n $k ${if (solution.isSuccess) "success" else "failure"}")
      println(s"took $took sec")
      if (took > 20) {
        println("TLE!!!!")
      }
    })
  }
}

object IndiciumTry2 {

  def buildPermutations(n: Int, array: Array[Array[Int]], currRow: Int, currCol: Int, leftToUse: Set[Int], currentPerm: Array[Int]): LazyList[Array[Int]] = {
    if(currCol == currRow){
      buildPermutations(n, array, currRow, currCol+1, leftToUse, currentPerm)
    }else{
      if (leftToUse.isEmpty) {
        if (currentPerm.count(_ > 0) == n) currentPerm.clone() #:: LazyList.empty else LazyList.empty
      } else {
        val prevRows: Set[Int] = Range(0, currRow).map(i => array(i)(currCol)).toSet
        val actualLeftToUse: Set[Int] = (leftToUse -- prevRows) -- Set(array(currCol)(currCol))

        actualLeftToUse.toList.map(y => {
          val currentPermClone = currentPerm.clone()
          currentPermClone(currCol) = y
          val leftToUseForNextIter: Set[Int] = leftToUse.filterNot(_ == y)
          buildPermutations(n, array, currRow, currCol + 1, leftToUseForNextIter, currentPermClone)
        }).fold(LazyList.empty)(_ #::: _)
      }
    }

  }

  def buildNaturalLatinSquare(n: Int, currMatrix: Array[Array[Int]], currRow: Int = 0): LazyList[Array[Array[Int]]] = {
    if (currRow == n) {
      currMatrix #:: LazyList.empty
    } else {
      val traceSetValue: Int = currMatrix(currRow)(currRow)
      val permutationStartingArr: Array[Int] = Array.ofDim(n)
      permutationStartingArr(currRow) = traceSetValue
      val validPermutations: LazyList[Array[Int]] = buildPermutations(n, currMatrix, currRow, 0, Range(1, n + 1).toSet.filterNot(_ == traceSetValue),permutationStartingArr)
      validPermutations.flatMap(permutation => {
          val newMatrix = currMatrix.clone()
         newMatrix(currRow) = permutation
          buildNaturalLatinSquare(n, newMatrix, currRow + 1)
      })
    }

  }

  def buildTracePermutations(n: Int, k: Int,  currPerm: Array[Int], currIdx: Int = 0, perms: mutable.Buffer[Array[Int]] = mutable.Buffer()): mutable.Buffer[Array[Int]] = {
    if(k<0 || (k==0 && currIdx!=n) || (k!=0 && currIdx>=n)){
      mutable.Buffer()
    }else if(k==0){
      val clonedPerm: Array[Int] = currPerm.clone()
      perms.append(clonedPerm)
      perms
    }else{
      Range(1, n+1).foreach(i => {
        currPerm(currIdx) = i
        buildTracePermutations(n, k-i, currPerm, currIdx+1, perms)
      })
      perms
    }
  }

  def buildTracePermutationsLazy(n: Int, k: Int,  currPerm: Array[Int], currIdx: Int = 0): LazyList[Array[Int]] = {
    if(k<0 || (k==0 && currIdx!=n) || (k!=0 && currIdx>=n)){
      LazyList.empty
    }else if(k==0){
      val clonedPerm: Array[Int] = currPerm.clone()
      clonedPerm #:: LazyList.empty
    }else{
      Range(1, n+1).map(i => {
        currPerm(currIdx) = i
        buildTracePermutationsLazy(n, k-i, currPerm, currIdx+1)
      }).fold(LazyList.empty)(_ #::: _)
    }
  }

  def solve(n: Int, k: Int): Try[Array[Array[Int]]] = {
    val tracePermutations: LazyList[Array[Int]] = buildTracePermutationsLazy(n, k, Array.ofDim(n))

    val naturalLatin: Try[Array[Array[Int]]] = if(tracePermutations.nonEmpty){
      var arr: Array[Array[Int]] = Array.ofDim(n, n)
      val res: LazyList[Array[Array[Int]]] = tracePermutations.flatMap(tracePerm => {
          arr = Array.ofDim(n, n)
          Range(0, n).foreach(i => {
            arr(i)(i) = tracePerm(i)
          })
          buildNaturalLatinSquare(n, arr)
      })

      res.headOption.map(Success(_)).getOrElse(Failure(new Exception()))
    }else {
      Failure(new Exception())
    }

    naturalLatin
  }

  def main(args: Array[String]): Unit = {
    val T: Int = StdIn.readInt()
    val res = Range(1, T + 1).flatMap(testIdx => {
      val Array(n, k): Array[Int] = StdIn.readLine().split(" ").map(_.toInt)

      val naturalLatin = solve(n, k)

      naturalLatin match {
        case Success(v) => List(s"Case #$testIdx: POSSIBLE") ++ v.map(_.mkString(" "))
        case Failure(exception) => List(s"Case #$testIdx: IMPOSSIBLE")
      }
    })

    res.foreach(println)

  }
}
