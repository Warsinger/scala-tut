package com.blogspot.kateel.scalatut


/**
 * Given 2 arrays, order the elements of the second array based on their order in the first array.
 * Ordering:   {'a', 'j', 'x', 'y', 'q', 'e', 'f'};
 * Input:      {'a', 'j', 'x', 'y', 'q', 'e', 'f', 'm', 'n', 'j', 'y', 'q', 'a', 'j', 'x', 'y', 'q', 'e', 'f'};
 * Expected:   {'a', 'a', 'j', 'j', 'j', 'x', 'x', 'y', 'y', 'y', 'q', 'q', 'q', 'e', 'e', 'f', 'f'};
 * Items not in the ordering are dropped.
 *
 * Created with IntelliJ IDEA.
 * User: mlee
 * Date: 1/30/14
 * Time: 12:41 PM
 */
object Ordering {

  def main(args: Array[String]) {
    val ordering = Array('a', 'j', 'x', 'y', 'q', 'e', 'f')
    val input = Array('a', 'j', 'x', 'y', 'q', 'e', 'f', 'm', 'n', 'j', 'y', 'q', 'a', 'j', 'x', 'y', 'q', 'e', 'f')
    val expected = Array('a', 'a', 'j', 'j', 'j', 'x', 'x', 'y', 'y', 'y', 'q', 'q', 'q', 'e', 'e', 'f', 'f')
    val order = new Ordering

    println("input: " + input.deep)
    println("expected: " + expected.deep)

    testResult("1", expected, input, ordering, order.orderArray1)
    testResult("2", expected, input, ordering, order.orderArray2)
    testResult("K", expected, input, ordering, order.orderArrayK)
  }

  def testResult(tag: String, expected: Array[Char], input: Array[Char], ordering: Array[Char], orderingFunc: (Array[Char], Array[Char]) => Array[Char]) {
    val output = orderingFunc.apply(input, ordering)
    if (output.sameElements(expected))
      printResult(tag, "correct", output)
    else
      printResult(tag, "incorrect", output)
  }


  def printResult(tag: String, correct: String, output: Array[Char]) {
    val deep = output.deep
    println(s"output $tag $correct: $deep")
  }
}

class Ordering {
  def orderArray1(input: Array[Char], ordering: Array[Char]): Array[Char] = {
    var result = Vector[Char]()
    ordering.foreach {
      c1 => input.foreach {
        c2 =>
          if (c2.equals(c1))
            result = result :+ c1
      }
    }
    result.toArray
  }

  def orderingComparator(c1: Char, c2: Char, orderMap: Map[Char, Int]): Boolean = {
    val i1 = orderMap.get(c1)
    val i2 = orderMap.get(c2)
    if (i1.isDefined && i2.isDefined) {
      return (i1.get - i2.get) < 0
    }

    if (i1.isEmpty) {
      return false
    }

    // i2 == null

    true
  }

  def orderArray2(input: Array[Char], ordering: Array[Char]): Array[Char] = {
    val orderMap = scala.collection.mutable.Map[Char, Int]()
    (0 until ordering.length).foreach {
      i => {
        orderMap += ordering(i) -> i
      }
    }
    val sorted = input.sortWith(orderingComparator(_, _, orderMap.toMap))

    sorted.filter {
      orderMap.contains(_)
    }
  }

  def orderArrayK(input: Array[Char], ordering: Array[Char]): Array[Char] =
    input.filter(c => ordering.indexOf(c) >= 0).sortBy(c => ordering.indexOf(c))
}
