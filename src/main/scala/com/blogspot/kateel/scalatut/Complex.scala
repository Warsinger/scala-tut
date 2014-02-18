package com.blogspot.kateel.scalatut

/**
 * Simple complex number class.
 *
 * Created with IntelliJ IDEA.
 * User: mlee
 * Date: 1/29/14
 * Time: 10:25 PM
 */
class Complex(real: Double, imaginary: Double) {
  def re = real

  def im = imaginary

  // overloaded vector addition
  def +(that: Complex): Complex =
    new Complex(this.re + that.re, this.im + that.im)

  // overloaded scalar addition
  def +(that: Double): Complex =
    new Complex(this.re + that, this.im)

  // overloaded scalar subtraction
  def -(that: Double): Complex =
    new Complex(this.re - that, this.im)

  // overloaded vector subtraction
  def -(that: Complex): Complex =
    new Complex(this.re - that.re, this.im - that.im)

  // overloaded scalar multiplication
  def *(that: Double): Complex =
    new Complex(this.re * that, this.im * that)

  // overloaded vector multiplication
  def *(that: Complex): Complex =   {
    // store FOIL results
    val first = this.re * that.re
    // imaginary part * imaginary part becomes negative
    val last = - this.im * that.im
    val outer = this.re * that.im
    val inner = this.im * that.re
    new Complex(first + last, outer + inner)
  }

  // overloaded unary negation
  def unary_- : Complex =
    new Complex(-this.re, -this.im)

  // overloaded ~ to find the magnitude of the vector
  def unary_~ : Double =
    Math.sqrt(Math.pow(this.re, 2) + Math.pow(this.im, 2))

  override def toString =
    re + (if (im < 0) "" else "+") + im + "i"
}

object ComplexNumbers {
  def main(args: Array[String]) {
    val c1 = new Complex(1.2, 3.4)
    println("imaginary part: " + c1.im)
    println(s"complex: $c1")
    val c2 = new Complex(1.5, 3.7)
    val c3 = c1 + c2
    println(s"add result: $c3")
    val c4 = c1 - c2
    println(s"vector subtract: $c4")
    val c5 = c1 * c2
    println(s"vector multiply: $c5")
    val c6 = -c1
    println(s"unary negation: $c6")
    val d1 = ~c1
    println(s"absolute value: $d1")
  }
}