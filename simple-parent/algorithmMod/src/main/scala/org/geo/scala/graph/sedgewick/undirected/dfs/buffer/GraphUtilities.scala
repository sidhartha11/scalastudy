package org.geo.scala.graph.sedgewick.undirected.dfs.buffer
import scala.io.Source
import scala.collection.mutable
import sys.process._
import java.net.URL
import java.io.File
import org.geo.scala.graph.GraphVertex
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.GlobalUtilities._

/**
 * GraphUtilities are contain utilities needed by a particular implementation
 * of the the Graph Object; dependent upon its underlying adjacency list structure.
 * So far I have 2 types: Map and Buffer
 */
object GraphUtilities {
  def requireVertexInGraph[T](graph: Graph[T], v: T): Boolean = {
    val t = graph.getGraph get v
    t != None
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
    for (key <- vertices) {
      for (edge <- graph.adj(key)) {
        if (key == edge)
          count += 1
      }
    }
    /**
     * the book said divide this number by 2 , however seems that using a map
     *  precludes the need to devide by two, each self loop can only be stored
     *  once.
     */
    count
  }

  /**
   * Create a graph object, populate with test file input
   * directional/undirectional based on biDir
   */
  def initializeGraph(filename: String, biDir: GraphConstants.Value): Graph[GraphVertex] = {
    clearCache
      initializeGraphInner(base.trim() + filename,biDir)
  }
        def initializeGraph(based: String , filename: String, biDir: GraphConstants.Value): Graph[GraphVertex] = {
    initializeGraphInner(based.trim() + filename, biDir)
  }
  def initializeGraphInner(filename: String, biDir: GraphConstants.Value): Graph[GraphVertex] = {
    /**
     * Create a Graph Object
     */
    val a: Graph[GraphVertex] = Graph(biDir)
    /**
     * Populate the graph with data from input file
     */
    println("adding nodes")
    var counter = 0
    var skipped = 0
    for ((node, neighbor, weight) <- readGraph(filename)) {
      var p = cache get (node + neighbor)
      if (p != None) {
        println("skipping duplicate:" + p)
        skipped += 1
      } else {
        counter += 1
        cache += ((node + neighbor) -> 0)
//        println("adding:(%s,%s,%d)".format(node, neighbor, weight))
        a.addEdge(GraphVertex(node, weight, false), GraphVertex(neighbor, weight, false))
      }

    }
    println("read #" + counter + " records, skipped #" + skipped )

    a
  }

}