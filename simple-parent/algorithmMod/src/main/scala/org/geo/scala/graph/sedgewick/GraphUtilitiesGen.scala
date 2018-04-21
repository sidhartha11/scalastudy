package org.geo.scala.graph.sedgewick

import scala.io.Source
import scala.collection.mutable
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.sedgewick.adjacency.Graph
import sys.process._
import java.net.URL
import java.io.File

/**
 * GraphUtilities are contain utilities needed by a particular implementation
 * of the the Graph Object; dependent upon its underlying adjacency list structure.
 * So far I have 2 types: Map and Buffer
 */
object GraphUtilitiesGen {
  def requireVertexInGraph[T](graph: Graph[T], v: T): Boolean = {
    val t = graph.getGraph get v
    t != None
  }

  def base =
    """|C:/githubstuff/javaprojs/studystuff
      |/scalastudy/simple-parent/algorithmMod
      |/src/main
      |/resources/
      """.stripMargin.replaceAll("\n", "")
  def filename = "cities.txt"
  def CITIES_CYCLE = "city_cycle.txt"
  def CITIES = "cities.txt"
  def NUMBERS = "numbers.txt"
  def TINYG = "tinyG.txt"

  def MEDIUMNUMBERS = "princetonMedium.txt"
  def PRINCETONLARGE = "princetonLarge.txt"
  def TINYGC        = "tinyCG.txt"
  /**
   * This method is used to read a list of graph data
   * Vertex, Vertex, edgeweight
   * This method seems to be inlined by the JVM
   * because of strange things happening inside the
   * for loop. It seems to be optimized by the JVM
   */
  def readGraph[T, W](file: String): List[(T, T, W)] = {
    println("reading file:" + file)
    val source = Source.fromFile(file)
    val triples = for (line <- source.getLines()) yield {
      val t = line split (",")
      (t(0).trim.asInstanceOf[T], t(1).trim.asInstanceOf[T], t(2).trim.asInstanceOf[W])
    }
    triples.toList
  }

  def downloadFile(urlname: String, filename: String): Unit = {

    new URL(urlname) #> new File(filename) !!
  }

  /**
   * degree of vertex v in the internal object representing the
   * adjacency list
   * The degree of a vertex is simply the size of its adjacency list
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

  def degree[T](vertices: mutable.LinkedHashMap[T, mutable.LinkedHashMap[T, T]], v: T): Int = {
    /** get the map entry in the vertex map for v **/
    val a = vertices get v
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

  def maxDegree[T](vertices: mutable.LinkedHashMap[T, mutable.LinkedHashMap[T, T]]): Int = {
    vertices.map(t => t._2.size).max
  }

  /**
   * I assume the average degree of the all vertices is
   * sum / numbervertices
   * @param graph
   * @return
   */
  def averageDegree[T](graph: Graph[T]): Int = {
    graph.getGraph.map(t => t._2.size).sum / graph.V
  }

  def numberOfSelfLoops[T](graph: Graph[T]): Int = {
    var count = 0
    val vertices = graph.getGraph.keySet
    /** loop thru each vertex **/
    for (key <- vertices) {
      /** get the edges for a particular vertex **/
      for (edge <- graph.adj(key)) {
        /** if the vertex is in the edge list, then it is a self loop **/
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
  def initializeGraph[T, W](filename: String, biDir: GraphConstants.Value): Graph[GraphVertexGen[T, W]] = {
    initializeGraphInner(base.trim() + filename, biDir)
  }
  def initializeGraph[T, W](based: String, filename: String, biDir: GraphConstants.Value): Graph[GraphVertexGen[T, W]] = {
    initializeGraphInner(based.trim() + filename, biDir)
  }
  def initializeGraphInner[T, W](filename: String, biDir: GraphConstants.Value): Graph[GraphVertexGen[T, W]] = {
    /**
     * Create a Graph Object
     */
    val a: Graph[GraphVertexGen[T, W]] = Graph(biDir)
    /**
     * Populate the graph with data from input file
     */
    println("adding nodes")
    var counter = 0
    var skipped = 0
    for ((node, neighbor, weight) <- readGraph[T, W](filename)) {
      counter += 1
      //        println("adding:(%s,%s,%d)".format(node, neighbor, weight))
      a.addEdge(GraphVertexGen[T, W](node, weight), GraphVertexGen[T, W](neighbor, weight))
    }
    println("read #" + counter + " records")
    a
  }

  /** utilities that load the graph with Graph object as input **/
  def loadGraph[T, W](base: String, filename: String, a: Graph[GraphVertexGen[T, W]]): Graph[GraphVertexGen[T, W]] = {
    loadGraphInner[T, W](base.trim() + filename, a)
  }

  def loadGraphInner[T, W](filename: String, a: Graph[GraphVertexGen[T, W]]): Graph[GraphVertexGen[T, W]] = {

    /**
     * Populate the graph with data from input file
     */
    println("adding nodes")
    var counter = 0
    var skipped = 0
    for ((node, neighbor, weight) <- readGraph[T, W](filename)) {
      counter += 1
      //        println("adding:(%s,%s,%d)".format(node, neighbor, weight))
      a.addEdge(GraphVertexGen[T, W](node, weight), GraphVertexGen[T, W](neighbor, weight))
    }
    println("read #" + counter + " records")
    a
  }

  /** utilities that load the vertices into a list only, for testing equals and compare functionality **/
  def loadGraph[T, W](base: String, filename: String): mutable.ArrayBuffer[GraphVertexGen[T, W]] = {
    loadGraphInner[T, W](base.trim() + filename)
  }

  def loadGraphInner[T, W](filename: String): mutable.ArrayBuffer[GraphVertexGen[T, W]] = {

    import scala.collection.mutable
    /**
     * Populate the graph with data from input file
     */
    println("Loading Vertex Data")
    var counter = 0
    var skipped = 0
    var list = mutable.ArrayBuffer[GraphVertexGen[T, W]]()
    for ((node, neighbor, weight) <- readGraph[T, W](filename)) {
      counter += 2
      list += GraphVertexGen(node, weight)
      list += GraphVertexGen(neighbor, weight)
      //        println("adding:(%s,%s,%d)".format(node, neighbor, weight))
    }
    println("read #" + counter + " records")
    list
  }
  def DEBUG = false
  def outPut(str: String) = {
    if (DEBUG) {
      println(str)
    }
  }
  def createAdjacencyMap[T, W](dir: GraphConstants.Value) = {
    Graph[GraphVertexGen[T, W]](dir)
  }

  def instantiateGraph[T, W](filename: String) = {
    println("creating adjacency structure")
    var adj = createAdjacencyMap[T, W](GraphConstants.undirected)
    if (DEBUG)
    outPut("adj=%s".format(adj))
    /**
     * load the structure with data
     */
    adj = loadGraph[T, W](base, filename, adj)
    adj
  }
  def elapsed(start: Long): Long = {
    (System.currentTimeMillis() - start)/1000
  }
}