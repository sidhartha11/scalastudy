package org.review.book.programminginscala3rd.pis_33

object Chapter33 {
  def testExpression(a:Arith , expr: String):Any = {
    println("input: " + expr)
    val parsedString = a.parseAll(a.expr,expr)
    parsedString
  }
  def main(args: Array[String]) {
    
      val a = new Arith()
      val testString = "2 * (3 + 7)("
      val parsedString = testExpression(a,testString)
      println("parsedString:%s".format(parsedString))
  }
}