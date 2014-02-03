package com.blogspot.kateel.scalatut

import collection.immutable.ListMap


/**
 * A little playground for learning scala features.
 *
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

    // uses ListMap to preserve ordering of the insertion order
    val funcs = ListMap("Double loop" -> order.orderArray1 _, "Map 1" -> order.orderArray2 _, "Map 1.1" -> order.orderArray3 _, "Kurt one liner" -> order.orderArrayK _)
    funcs.foreach(tuple => testResult(tuple._1, tuple._2))

    def testResult(tag: String, orderingFunc: (Array[Char], Array[Char]) => Array[Char]) {
      val output = orderingFunc.apply(input, ordering)
      if (output.sameElements(expected))
        println(formatString(tag, "correct", output))
      else
        throw new IllegalStateException(formatString(tag, "incorrect", output))
    }
  }

  def formatString(tag: String, status: String, output: Array[Char]): String = {
    val deep = output.deep
    s"output for ordering function tagged '$tag' is $status: $deep"
  }
}

class Ordering {
  /**
   * Simple double array traversal that builds a list of things that match
   * @param input the input array to order
   * @param ordering the array to use to define the order and allowable values
   * @return an array where the elements are ordered by the ordering array and only those elements contained in the ordering array are present
   */
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

  /**
   * Builds a map of the ordering array keyed on the character with the value being the index in the array. Sorts the array
   * using a comparator then filters out the sorted array based on its existence in the map
   * @param input the input array to order
   * @param ordering the array to use to define the order and allowable values
   * @return an array where the elements are ordered by the ordering array and only those elements contained in the ordering array are present
   */
  def orderArray2(input: Array[Char], ordering: Array[Char]): Array[Char] = {
    val orderMap = scala.collection.mutable.Map[Char, Int]()
    (0 until ordering.length).foreach {
      i => {
        orderMap += ordering(i) -> i
      }
    }
    input.sortWith(orderingComparator(_, _, orderMap.toMap)).filter {
      orderMap.contains(_)
    }
  }

  /**
   * Builds a map of the ordering array keyed on the character with the value being the index in the array. Filters the array
   * based on its existence in the map and then sorts the array using a comparator that orders things by the values in the map (reverse order of operations from orderArray2)
   * @param input the input array to order
   * @param ordering the array to use to define the order and allowable values
   * @return an array where the elements are ordered by the ordering array and only those elements contained in the ordering array are present
   */
  def orderArray3(input: Array[Char], ordering: Array[Char]): Array[Char] = {
    val orderMap = scala.collection.mutable.Map[Char, Int]()
    (0 until ordering.length).foreach {
      i => {
        orderMap += ordering(i) -> i
      }
    }
    input.filter(orderMap.contains(_)).sortWith(orderingComparator(_, _, orderMap.toMap))
  }

  /**
   * Filters the array based on its presence in the ordering array, then sorts by the ordering array.
   * @param input the input array to order
   * @param ordering the array to use to define the order and allowable values
   * @return an array where the elements are ordered by the ordering array and only those elements contained in the ordering array are present
   */
  def orderArrayK(input: Array[Char], ordering: Array[Char]): Array[Char] =
    input.filter(c => ordering.indexOf(c) >= 0).sortBy(c => ordering.indexOf(c))
}
