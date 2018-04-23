package org.geo.scala.graph

import scala.io.Source
import scala.collection.mutable
import sys.process._
import java.net.URL
import java.io.File
import java.util.concurrent.ThreadLocalRandom



object GlobalUtilities {
  var cache: mutable.Map[String, Int] = mutable.Map[String, Int]()

  def PRINTRANDOM = false
  def clearCache : Unit ={
    println("clearing cache")
    cache.clear
  }
  def base =
    """|C:/githubstuff/javaprojs/studystuff
      |/scalastudy/simple-parent/algorithmMod
      |/src/main
      |/resources/
      """.stripMargin.replaceAll("\n", "")
  def filename = "cities.txt"

  /**
   * This method is used to read a list of graph data
   * Vertex, Vertex, edgeweight
   * This method seems to be inlined by the JVM
   * because of strange things happening inside the
   * for loop. It seems to be optimized by the JVM
   */
  def readGraph(file: String): List[(String, String, Int)] = {
    println("reading file:" + file)
    val source = Source.fromFile(file)
    val triples = for (line <- source.getLines()) yield {
      val t = line split (",")
      (t(0).trim, t(1).trim, t(2).trim.toInt)
    }
    source.close()
    triples.toList
  }

  def downloadFile(urlname: String, filename: String): Unit = {

    new URL(urlname) #> new File(filename) !!
  }
  
  def readFile(file: String): mutable.ArrayBuffer[String] = {
       println("reading file:" + file)
    val source = Source.fromFile(file)
    var buf = mutable.ArrayBuffer[String]()
    for (line <- source.getLines())  {
      // println(line)
      buf += line.trim
    }
    source.close()
    buf
  }
  
  def syntheticDataRaw[T,W](filename: String): List[(T, T, W)] = {
      val input = readFile(filename)
      val buf = mutable.ArrayBuffer[(T,T,W)]()
      for ( i <- 0 until 400000  ){
        val left = ThreadLocalRandom.current().nextInt(0,input.size)
        val right =ThreadLocalRandom.current().nextInt(0,input.size)
        val weight =ThreadLocalRandom.current().nextInt(100,201)
        buf += (( input(left).asInstanceOf[T], input(right).asInstanceOf[T], weight.asInstanceOf[W] ))
      }
      if (PRINTRANDOM) {
        println("dumping random values")
        for ( b <- buf ) println(b._1 + "^" + b._2 + "^" + b._3)
      }
      buf.toList
  }
  
    def syntheticData[T,W](filename: String): List[(T, T, W)] = {
      val input = readFile(filename)
      val buf = mutable.ArrayBuffer[(T,T,W)]()
      for ( i <- input ){
 //       println("i=%s, i.size=%d, %s".format(i,i.size, i.getClass))
        val elements = i split("\\^")
//        println(elements)
        val left = elements(0)
        val right = elements(1)
        val weitht = elements(2)
        val weight =ThreadLocalRandom.current().nextInt(100,201)
        buf += (( left.asInstanceOf[T], right.asInstanceOf[T], weight.asInstanceOf[W] ))
      }
      if (PRINTRANDOM) {
        println("dumping random values")
        for ( b <- buf ) println(b._1 + "^" + b._2 + "^" + b._3)
      }
      buf.toList
  }

  def main(args:Array[String]) {
    val b = syntheticData("C:\\temp\\rawCities.txt")
    for ( l <- b ) println("left=%s, right=%s,weight=%d".format(l._1 , l._2, l._3))
  }
}