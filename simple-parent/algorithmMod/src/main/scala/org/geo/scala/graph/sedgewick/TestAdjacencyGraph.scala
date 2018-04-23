package org.geo.scala.graph.sedgewick

import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._

import scala.collection.mutable

/**
 * <pre>
 * This object is used to test an Adjacency map that is to be
 * used in performing various types of graphing applications:
 * 1. Finding cycles in a graph.
 * 2. Finding connected paths in a graph.
 * 3. Finding shortest path in a undirected and directed graph.
 *
 * This object is only meant the test the Adjacency part of the
 * application.
 * 
 * There are several different types of adjacency implementations. I am 
 * using a Map implementation. Following are the time and space complexities
 * of various implementations:
 * data         space    add edge    check whether w      iterate thru vertices
 * structure             v-w	       is adjacent to v     adjacent to v
 * 
 * list of      E        l           E                    D 
 * edges 
 * 
 * adjacency    v(sq)    l           l                    V
 * matrix 
 * 
 * adjacency    E+V      l           degree(v)            degree(v)
 * lists
 * 
 * adjacency
 * sets         E+V      logV        logV                 degree(v)      
 * 
 * </pre>
 */
object TestAdjacencyGraph {
  /** test data **/
  def CITIES_CYCLE = "city_cycle.txt"
  def CITIES = "cities.txt"
  def NUMBERS = "numbers.txt"
  def MEDIUMNUMBERS = "princetonMedium.txt"

  // def filename = "numbers.txt"
  def testCreateAdjacencyMap[T, W](dir: GraphConstants.Value) = {
    Graph[GraphVertexGen[T, W]](dir)
  }

  def testLoadingLargeNumbers = {
    /** create adjacency structure **/
    println("loading large numbers")
    var adj = testCreateAdjacencyMap[Int, Int](GraphConstants.undirected)
    println("adj=%s".format(adj))
    /**
     * load the structure with data
     */
    adj = loadGraph[Int, Int](",")("C:\\temp\\", MEDIUMNUMBERS, adj)

    /** display the vertex map: Map(vertex , adjacency map ) **/
    for (v <- adj.getGraph) {
      println(v)
    }
  }

  def testLoadingCities = {
    /** create adjacency structure **/
    println("loading Cities")
    var adj = testCreateAdjacencyMap[String, Int](GraphConstants.undirected)
    println("adj=%s".format(adj))
    /**
     * load the structure with data
     */
    adj = loadGraph[String, Int](",")(base, CITIES, adj)

    /** display the vertex map: Map(vertex , adjacency map ) **/
    for (v <- adj.getGraph) {
      println(v)
    }
  }

  def testLoadingDataIntoBuffer {
    val l = loadGraph[String, Int](base, CITIES)
    println("\nunsorted\n")
    for (x <- l) println(x)
    println("\nsorted\n")
    l.sortBy(_.name).foreach(println)
  }

  def testAdjacencyFunctionalityNumbers = {
		  /** create adjacency structure **/
    println("loading large numbers")
    var adj = testCreateAdjacencyMap[Int, Int](GraphConstants.undirected)
    println("adj=%s".format(adj))
    /**
     * load the structure with data
     */
    adj = loadGraph[Int, Int](",")(base, NUMBERS, adj)

    /**
     * Get the degree of each vertex in the
     * vertex map
     */
    /** get the internal graph **/
    val graph : mutable.LinkedHashMap[GraphVertexGen[Int, Int], mutable.LinkedHashMap[GraphVertexGen[Int, Int], GraphVertexGen[Int, Int]]] = adj.getGraph
    /** dump each vertex with its degree **/
    for ( ( k , v ) <- graph ) {
      println("vertex:%s,degree=%d".format(k, v.size))
    }
    
    /** get the Maximum Degree of all vertices **/
    println("maxDegree = %d".format(maxDegree(graph)))
    
    /** get the average degree of the vertices **/
    println("averageDegree = %d".format(averageDegree(adj)))
    /** testprintgraph **/
    adj.printGraph
  }
  
    def testAdjacencyFunctionalityCities = {
		  /** create adjacency structure **/
    println("loading large numbers")
    var adj = testCreateAdjacencyMap[String, Int](GraphConstants.undirected)
    println("adj=%s".format(adj))
    /**
     * load the structure with data
     */
    adj = loadGraph[String, Int](",")(base, CITIES, adj)

    /**
     * Get the degree of each vertex in the
     * vertex map
     */
    /** get the internal graph **/
    val graph : mutable.LinkedHashMap[GraphVertexGen[String, Int], mutable.LinkedHashMap[GraphVertexGen[String, Int], GraphVertexGen[String, Int]]] = adj.getGraph
    /** dump each vertex with its degree **/
    for ( ( k , v ) <- graph ) {
      println("vertex:%s,degree=%d".format(k, v.size))
    }
    
    /** get the Maximum Degree of all vertices **/
    println("maxDegree = %d".format(maxDegree(graph)))
    
    /** get the average degree of the vertices **/
    println("averageDegree = %d".format(averageDegree(adj)))
    
    /** get the number of self loops **/
    println("numberOfSelfLoops = %d".format(numberOfSelfLoops(adj)))
    
    /** print number of vertices and number of edges **/
    println("#vertices = %d, #edges = %d".format(adj.V, adj.E))
    /** testprintgraph **/
    adj.printGraph
  }
  def main(args: Array[String]) {
    testAdjacencyFunctionalityNumbers
  }
}