package org.geo.scala.graph.sedgewick

import java.io.File
import java.io.FileNotFoundException
import java.net.URL

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.sys.process._
import scala.util.Try

import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.sedgewick.adjacency.Graph
import scala.io.BufferedSource
import scala.io.Codec
import java.io.InputStream
import java.io.BufferedInputStream
import java.io.BufferedReader

/**
 * GraphUtilities are contain utilities needed by a particular implementation
 * of the the Graph Object; dependent upon its underlying adjacency list structure.
 * So far I have 2 types: Map and Buffer
 */
object GraphUtilitiesGen {


  
  implicit class OpsNum(val str: String) extends AnyVal {
    def isNumeric() = scala.util.Try(str.toDouble).isSuccess
  }

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
  def DEBUG = false
  def filename = "cities.txt"
  def CITIES_NOCYCLE = "cities_nocycle.txt"
  def CITIES_CYCLE = "city_cycle.txt"
  def TWOCOLOR = "twocolor.txt"
  def CITIES = "cities.txt"
  def NUMBERS = "numbers.txt"
  def TINYG = "tinyG.txt"
  def RAWCITIES = "rawCities.txt"
  def SYNTHETICCITIES = "syntheticCities.txt"

  def MEDIUMNUMBERS = "princetonMedium.txt"
  def PRINCETONLARGE = "princetonLarge.txt"
  def TINYGC = "tinyCG.txt"
  /**
   * This method is used to read a list of graph data
   * Vertex, Vertex, edgeweight. The BufferedSource represents an object
   * to read from the file system or the class path
   */

  def readGraph[T, W](delem: String)(file: String, source: BufferedSource): ArrayBuffer[(T, T, W)] = {
    println("delem=" + delem)
    var buffer = ArrayBuffer[(T, T, W)]()
    for (line <- source.getLines()) {
      val t = line split ("\\" + delem)
      if (t.size != 3) {
        println("skipping , 3 elements required, v delim v delim wt" + line)
      } else {
        /**
         * simple syntax check making sure the the third
         * field is numeric
         */
        if (t(2) isNumeric ()) {
          buffer += ((t(0).trim.asInstanceOf[T], t(1).trim.asInstanceOf[T], t(2).trim.asInstanceOf[W]))
        } else {
          println("skipped, 3rd field not numeric")
        }
      }
    }
    buffer
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

  /** utilities that load the graph with Graph object as input **/
  /** to load randomized synthetic city data use <methodname>Random **/

  def fullPath(filename: String) = {
    val namePattern = "^[A-Z]:|^\\/".r
    println("analyzing filename:" + filename)
    /** see if first occurrence if a full path name **/
    val b = namePattern.findFirstIn(filename)
    b != None
  }
  def loadGraph[T, W](delem: String)(filename: String, a: Graph[GraphVertexGen[T, W]]): Graph[GraphVertexGen[T, W]] = {
    /**
     * If filename begins with a "/" then do not append the base because it is a
     * complete filename and not located in the resources directory.
     */
    /** set up the input first **/
    var input: BufferedSource = null
    if (fullPath(filename)) {
      /** this is on the file system **/
      input = Source.fromFile(filename)
      println("reading file:" + filename + " from filesystem ")
    } else {
      /** simple file name assumed from class path **/
      input = Source.fromResource(filename)
      println("reading file:" + filename + " from classpath ")

    }

    /**
     * Populate the graph with data from input file
     */
    println("adding nodes")
    var counter = 0
    var skipped = 0
    for ((node: T, neighbor: T, weight: W) <- readGraph[T, W](delem)(filename, input)) {
      counter += 1
      if (DEBUG) {
        println("adding:(%s,%s,%s)".format(node, neighbor, weight))
      }
      a.addEdge(GraphVertexGen[T, W](node, weight), GraphVertexGen[T, W](neighbor, weight))
    }
    println("read #" + counter + " records")
    a
  }

  def outPut(str: String) = {
    if (DEBUG) {
      println(str)
    }
  }
  def createAdjacencyMap[T, W](dir: GraphConstants.Value) = {
    Graph[GraphVertexGen[T, W]](dir)
  }
  /** for created the default digraph **/
  def createAdjacencyMap[T, W]() = {
    Graph[GraphVertexGen[T, W]]()
  }

  def instantiateGraph[T, W](delem: String)(filename: String, dir: GraphConstants.Value = GraphConstants.undirected) = {
    println("creating adjacency structure:" + dir)
    var adj = createAdjacencyMap[T, W](dir)
    if (DEBUG)
      outPut("adj=%s".format(adj))
    /**
     * load the structure with data
     */
    adj = loadGraph[T, W](delem)(filename, adj)
    adj
  }

  def conversion1(delemfrom: String)(delemto: String)(filename: String) = {
    println("converting file format:" + filename)
    var input: BufferedSource = null
    if (fullPath(filename)) {
      /** this is on the file system **/
      input = Source.fromFile(filename)
      println("reading file:" + filename + " from filesystem ")
    } else {
      /** simple file name assumed from class path **/
      input = Source.fromResource(filename)
      println("reading file:" + filename + " from classpath ")

    }
    val outer = for (line <- input.getLines()) yield {
      val t = line split ("\\" + delemfrom)
      val vertex = t head
      val list = for (remaining <- t tail) yield {
//        println("vertex=" + vertex + ", remaining=" + remaining)
        (vertex, remaining, 100)
      }
      list
    }
    outer
  }

  def elapsed(start: Long): Long = {
    (System.currentTimeMillis() - start) / 1000
  }
}
object GraphUtilRunner {
  import GraphUtilitiesGen._
  def main(args: Array[String]) {
    val t = conversion1("/")(",")("C:\\books\\algs4-data\\algs4-data\\jobs.txt")
   
    println("*****dumping")
    for ( i <- t ) {
     for ( a <- i ) {
       println(a)
     }
   }
  }
}