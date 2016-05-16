package cuttingProfit

class RodCutter {

  def findOptimalCuts(length: Int, prices: Map[Int, Int]): (Int, List[List[Int]]) = length match {
    case 0 => (0, List(List()))
    case _ =>
      val uncutPrice = if(prices.contains(length)) (prices(length), List(List(length))) else (0, List(List()))
      (1 to length-1).foldLeft(uncutPrice) {(cuts, split) => determineBestCuts(cuts, findOptimalCutsForSplit(split, length-split, prices))}
  }

  def determineBestCuts(cutsOne: (Int, List[List[Int]]), cutsTwo: (Int, List[List[Int]])): (Int, List[List[Int]]) = cutsOne._1 match {
      case matcher: Int if matcher > cutsTwo._1  => cutsOne
      case matcher: Int if matcher == cutsTwo._1 =>
        val combined = cutsOne._2 ::: cutsTwo._2
        (cutsOne._1, combined.distinct)
      case _ => cutsTwo
   }

  def findOptimalCutsForSplit(splitOne: Int, splitTwo: Int, prices: Map[Int, Int]):(Int, List[List[Int]]) ={
    val splitOneResult = if(prices.contains(splitOne))  findOptimalCuts(splitOne, prices) else (0, List(List()))
    val splitTwoResult = if(prices.contains(splitTwo))  findOptimalCuts(splitTwo, prices) else (0, List(List()))
    val combinedCuts = splitOneResult._2.flatMap(splitOneList => splitTwoResult._2.map(splitTwoList => splitOneList ::: splitTwoList))

    (splitOneResult._1 + splitTwoResult._1, combinedCuts)
  }
}