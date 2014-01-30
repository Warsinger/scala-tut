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
    val input1 = Array('a', 'j', 'x', 'y', 'q', 'e', 'f', 'm', 'n', 'j', 'y', 'q', 'a', 'j', 'x', 'y', 'q', 'e', 'f')
    val input2 = input1.clone()
    val expected = Array('a', 'a', 'j', 'j', 'j', 'x', 'x', 'y', 'y', 'y', 'q', 'q', 'q', 'e', 'e', 'f', 'f')
    val order = new Ordering
    val output1 = order.orderArray1(input1, ordering)
    if (output1.sameElements(expected))
      println("output 1 correct")
    else
      println("output 1 incorrect")
    val output2 = order.orderArray2(input2, ordering)
    if (output2.sameElements(expected))
      println("output 2 correct")
    else
      println("output 2 incorrect")

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
}
