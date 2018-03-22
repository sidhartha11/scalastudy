package org.geo.scala.graph.undirected.map

import scala.io.Source
import scala.collection.mutable

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
  def readGraph(file: String): List[(String, String, Int)] = {
    val source = Source.fromFile(base.trim() + file)
    val triples = for (line <- source.getLines()) yield {
      val t = line split (",")
      (t(0).trim, t(1).trim, t(2).trim.toInt)
    }
    triples.toList
  }

  /**
   * degree of vertex v in the internal object representing the 
   * adjacency list
   * @param graph The Graph trait object 
   * @param v The Vertex being inpspected
   * @return The degree of this vertex
   */
def degree[T](graph: Graph[T], v: T): Int = {
  /** get the map entry in the vertex map for v **/
    val a = graph.getGraph get v
    var i: Int = 0
    if (a == None) 0
    
    /** get the value from the Option and get its size **/
    a.get.size
  }

  /**
   * The vertex with the largest degree 
   * @param graph The Graph trait object
   * @return the max degree of all vertices 
   */
def maxDegree[T](graph: Graph[T]): Int = {
    graph.getGraph.map(t => t._2.size).max
  }

  /**
   * The average Vertex degree based on internal variables collecting
   * the # of vertices and # of edges
   * @param graph
   * @return
   */
def averageDegree[T](graph: Graph[T]): Int = {
    2 * (graph.E / graph.V)
  }

def numberOfSelfLoops[T](graph: Graph[T]): Int = {
  var count = 0
  val vertices = graph.getGraph.keySet
  for ( key <- vertices ) {
    for ( edge <- graph.adj(key)) {
      if ( key == edge )
        count += 1
    }
  }
  /** the book said divide this number by 2 , however seems that using a map
   *  precludes the need to devide by two, each self loop can only be stored
   *  once. 
   */
  count
}
  /** METHODS BELOW ARE NOT BEING USED AND WILL BE REMOVED **/
  def degreeNotUsed[T](graph: mutable.Map[T, mutable.Map[T, T]], v: T): Int = {
    val a = graph get v
    var i: Int = 0
    if (a == None) 0
    a.get.size
  }

  def maxDegreeNotUsed[T](graph: mutable.Map[T, mutable.Map[T, T]]): Int = {
    var i: Int = 0
    for (vertex <- graph) {
      if (vertex._2.size > i)
        i = vertex._2.size
    }
    i
  }

}