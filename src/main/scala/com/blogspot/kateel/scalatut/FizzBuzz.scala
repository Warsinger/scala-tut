package com.blogspot.kateel.scalatut


/**
 * print #s from 1-100 if divisible by 3 print "fizz" if divisible by 5 print "buzz" if divisible by both then print "fizzbuzz"

 * Created with IntelliJ IDEA.
 * User: mlee
 * Date: 1/30/14
 * Time: 11:22 AM
 */
object FizzBuzz {


  def main(args: Array[String]) {
    printFizzBuzz(1000)
  }

  def listFizzBuzz(count: Int): Seq[String] = {
    (1 to count) map {
      i => evalNum(i)
    }
  }

  def evalNum(i: Int): String = {
    val sb = new StringBuilder
    var divisible = false
    if (i % 3 == 0) {
      sb ++= "fizz"
      divisible = true
    }
    if (i % 5 == 0) {
      sb ++= "buzz"
      divisible = true
    }
    if (!divisible)
      sb ++= i.toString
    sb.toString()
  }

  def printFizzBuzz(count: Int) {
    listFizzBuzz(count).foreach {
      x => println(x)
    }
  }
}
