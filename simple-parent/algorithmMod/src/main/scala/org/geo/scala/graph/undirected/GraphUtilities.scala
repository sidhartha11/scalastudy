package org.geo.scala.graph.undirected

import scala.io.Source

object GraphUtilities {
  def base =
    """|C:/githubstuff/javaprojs/studystuff
      |/scalastudy/simple-parent/algorithmMod
      |/src/main/
      |/resources/
      """.stripMargin.replaceAll("\n", "")
  def filename = "graph.txt"

  /**
   * This method is used to read a list of graph data
   * Vertex, Vertex, edgeweight
   */
  def readGraph(file: String): List[(String,String,Int)] = {
    val source = Source.fromFile(base.trim() + file)
    val triples = for (line <- source.getLines()) yield {
      val t = line split (",")
      (t(0).trim,t(1).trim,t(2).trim.toInt)
    }
    triples.toList
  }
}