package org.geo.scala.graph.sedgewick

import java.io.File
import java.io.FileNotFoundException
import java.net.URL

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.sys.process._
import scala.util.Try

import org.geo.scala.graph.sedgewick.GraphUtilitiesGen.fullPath
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.sedgewick.adjacency.Graph
import scala.io.BufferedSource
import scala.io.Codec
import java.io.InputStream
import java.io.BufferedInputStream
import java.io.BufferedReader
import org.geo.scala.graph.TestTrait
import org.geo.scala.graph.sedgewick.adjacency.EdgeWeightedGraph
import org.geo.scala.graph.GraphVertex

/**
 * GraphUtilities are contain utilities needed by a particular implementation
 * of the the Graph Object; dependent upon its underlying adjacency list structure.
 * So far I have 2 types: Map and Buffer
 */


object GraphUtilitiesEdge {
trait MyOrdering[W] {
  def compare(x: W , y: W): W
}
/**
 * Note: Implicit objects cannot be in top level
 */
//implicit object DoubleOrdering extends  MyOrdering[TestTrait[GraphVertex[String],Double]] {
//  def compare ( x: TestTrait[GraphVertex[String],Double], y: TestTrait[GraphVertex[String],Double]): Double = {
//    x.weight - y.weight
//  }
//}

  /** NEWONE **/
  implicit class OpsNum(val str: String) extends AnyVal {
    def isNumeric() = scala.util.Try(str.toDouble).isSuccess
  }


  def DEBUG = true
  /**
   * This method is used to read a list of graph data
   * Vertex, Vertex, edgeweight. The BufferedSource represents an object
   * to read from the file system or the class path
   */
  /** NEWONE **/
  def readGraph(delem: String)(file: String, source: BufferedSource): ArrayBuffer[(String,String,Double)] = {
    println("delem=" + delem)
    var buffer = ArrayBuffer[(String,String,Double)]()
    for (line <- source.getLines()) {
      val t = line split ("\\" + delem)
      println("t = " + t ) 
      println("line = " + line )
      println("t.size = " + t.size)
      if (t.size != 3) {
        println("skipping , 3 elements required, v delim v delim wt" + line)
      } else {
        /**
         * simple syntax check making sure the the third
         * field is numeric
         */
        if (t(2) isNumeric ()) {
          buffer += ((t(0), t(1), t(2).trim.toDouble))
        } else {
          println("skipped, 3rd field not numeric")
        }
      }
    }
    buffer
  }

  /** NEWONE **/
//  def loadGraph[T,W <: Ordered[W]](delem: String)(filename: String 
  def loadGraph(delem: String)(filename: String 

      ,edgeGraph: EdgeWeightedGraph[GraphVertex[String],Double]): EdgeWeightedGraph[GraphVertex[String],Double] = {
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
    for ((node: String, neighbor: String, weight: Double) <- readGraph(delem)(filename, input)) {
      counter += 1
      if (DEBUG) {
        println("adding:(%s,%s,%s)".format(node, neighbor, weight))
      }
      val v = GraphVertex[String](node)
      val w = GraphVertex[String](neighbor)
      val edge = TestTrait[GraphVertex[String],Double ](v,w,weight)
      edgeGraph.addEdge(edge)

    }
    println("read #" + counter + " records")
    edgeGraph
  }

  def createEdgeGraph(dir: GraphConstants.Value) = {
    EdgeWeightedGraph[GraphVertex[String],Float](dir)
  }
  /** NEWONE **/
//  def instantiateGraph[T,W <: Ordered[W]](delem: String)(filename: String, dir: GraphConstants.Value = GraphConstants.undirected) = {
  def instantiateGraph(delem: String)(filename: String, dir: GraphConstants.Value = GraphConstants.undirected) = {
    
    println("creating adjacency structure:" + dir)
    
    var edgeGraph = EdgeWeightedGraph[GraphVertex[String],Double](GraphConstants.directed)

    if (DEBUG)
      println("adj=%s".format(edgeGraph))
    /**
     * load the structure with data
     */
    edgeGraph = loadGraph(delem)(filename, edgeGraph)
    edgeGraph
  }
}