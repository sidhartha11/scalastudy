package org.geo.scala.graph.undirected.map

import GraphUtilities._
object GraphTester {
  def main(args: Array[String]) {
    /**
     * Create a Graph Object
     */
    val a: Graph[GraphVertex] = Graph(GraphConstants.undirected)
    println("a.V=%d,a.E=%d".format(a.V, a.E))

    /**
     * Populate the graph with data from input file
     */
    for ((node, neighbor, weight) <- GraphUtilities.readGraph("dummy.txt")) {
      println("adding:(%s,%s,%d)".format(node, neighbor, weight))
      a.addEdge(GraphVertex(node, weight, false), GraphVertex(neighbor, weight, false))
    }

    /**
     * Print out the underlying data-structure containing the graph
     */
    println("printing graph via printGraph")
    a.printGraph
    println("#vertices:%d, #edges:%d".format(a.V, a.E))

    /**
     * print max Degree of the graph's
     */
    println("maxDegree of a is %d".format(maxDegree(a)))
    println("print iterable of each Vertex:")
    for (v <- a.getGraph) {
      println("degree of vertex:%s=%d".format(v._1, degree(a, v._1)))
      println("interable of %s".format(v._1))
      println(a.adj(v._1))
    }

    /** average degree **/
    println("average degree:%d".format(averageDegree(a)))

    /**
     * Print number of self loops
     */
    println("number self loops:%d".format(numberOfSelfLoops(a)))
  }
}