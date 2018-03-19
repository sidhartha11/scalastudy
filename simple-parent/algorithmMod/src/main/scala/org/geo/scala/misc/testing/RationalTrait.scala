package org.geo.scala.misc.testing

trait RationalTrait {
  val numerArg: Int
  val denomArg: Int
  require(denomArg != 0)

  private val g = gcd(numerArg, denomArg)

  val numer = numerArg / g
  val denom = denomArg / g

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  override def toString = numer + "/" + denom
}

object Runner {

  def testDoesNotWork = {
    val x = 2
    val tr = new RationalTrait /** the section below is considered a class **/ {
      val numerArg = 1 * x
      val denomArg = 2 * x
    }
    println("tr = " + tr.denomArg)
  }

  def main(args: Array[String]) {
    val x = 2
//    val tr = new {
//      val numerArg = 1 * x
//      val demomArg = 2 * x
//    } with RationalTrait

//    println("tr = " + tr.denomArg)
    testDoesNotWork

  }

}