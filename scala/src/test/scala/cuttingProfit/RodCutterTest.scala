package cuttingProfit

import org.junit.Assert._
import org.scalatest.FunSuite
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar

class RodCutterTest extends FunSuite with MockitoSugar {

  val rodCutter = new RodCutter
  val prices = Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4)

  test("Canary"){
    assert(true)
  }

  test("find Optimal Cuts For 0"){
    val expected = (0, List(List()))

    assertEquals(expected, rodCutter.findOptimalCuts(0, prices))
  }

  test("find Optimal Cuts For 1"){
    val expected = (1, List(List(1)))

    assertEquals(expected, rodCutter.findOptimalCuts(1, prices))
  }

  test("find Optimal Cuts For Split Combines Cuts"){
    val mockCutter = mock[RodCutter]
    when(mockCutter.findOptimalCuts(1, prices)).thenReturn((1, List(List(1))))
    when(mockCutter.findOptimalCuts(2, prices)).thenReturn((4, List(List(2))))
    when(mockCutter.findOptimalCutsForSplit(1, 2, prices)).thenCallRealMethod()

    val expected = (5, List(List(1, 2)))

    assertEquals(expected, mockCutter.findOptimalCutsForSplit(1, 2, prices))
  }

  test("find Optimal Cuts For 2") {
    val expected = (2, List(List(2), List(1, 1)))

    assertEquals(expected, rodCutter.findOptimalCuts(2, prices))
  }

  test("find Optimal Cuts For 2 with Different prices"){
    val expected = (6, List(List(1, 1)))
    val prices = Map(1 -> 3, 2 -> 2)

  assertEquals(expected, rodCutter.findOptimalCuts(2, prices))
}

  test("find Optimal Cuts For 3 with price Missing 1"){
    val expected = (3, List(List(3)))
    val prices = Map(2-> 2, 3 -> 3)

    assertEquals(expected, rodCutter.findOptimalCuts(3, prices))
  }

  test("find Optimal Cuts For 3 with price Missing 2"){
    val expected = (3, List(List(3)))
    val prices = Map(1 -> 1, 3 -> 3)

    assertEquals(expected, rodCutter.findOptimalCuts(3, prices))
  }

  test("find Optimal Cuts For 2 with price Missing self"){
    val expected = (6, List(List(1, 1)))
    val prices = Map(1 -> 3)

    assertEquals(expected, rodCutter.findOptimalCuts(2, prices))
  }

  test("find Optimal Cuts For 3"){
    val expected = (3,List(List(3), List(1, 2), List(1, 1, 1), List(2, 1)))

    assertEquals(expected, rodCutter.findOptimalCuts(3, prices))
  }

  test("find Optimal Cuts for 4") {
    val expected = (4,List(List(4), List(1, 3), List(1, 1, 2), List(1, 1, 1, 1), List(1, 2, 1), List(2, 2), List(2, 1, 1), List(3, 1)))

    assertEquals(expected, rodCutter.findOptimalCuts(4, prices))
  }
}
