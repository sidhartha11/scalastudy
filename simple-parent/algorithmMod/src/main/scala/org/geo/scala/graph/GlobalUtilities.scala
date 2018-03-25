package org.geo.scala.graph

import scala.io.Source
import scala.collection.mutable
import sys.process._
import java.net.URL
import java.io.File


object GlobalUtilities {
  var cache: mutable.Map[String, Int] = mutable.Map[String, Int]()

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
    triples.toList
  }

  def downloadFile(urlname: String, filename: String): Unit = {

    new URL(urlname) #> new File(filename) !!
  }

}