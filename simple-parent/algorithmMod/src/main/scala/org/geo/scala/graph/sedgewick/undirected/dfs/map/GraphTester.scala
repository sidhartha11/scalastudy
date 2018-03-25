package org.geo.scala.graph.sedgewick.undirected.dfs.map

import GraphUtilities._
import org.geo.scala.graph.GlobalUtilities._
import org.geo.scala.graph.GraphConstants._
import org.geo.scala.graph.GraphVertex


object GraphTester {
  /**
   * test general graph functionality
   * Large file to test can be found here:
   * https://algs4.cs.princeton.edu/41graph/largeG.txt
   */
  def testGeneralFunctionality = {
    /** create a graph object **/
    val a = initializeGraph("C:\\temp\\", "princetonLarge.txt", undirected)
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

  def testShowAllConnectionsInGraph = {
        /** create a graph object **/
    val a = initializeGraph("dummy.txt", undirected)
    println("\nmaxDegree of a is %d".format(maxDegree(a)))
    println("\nprinting graph via printGraph")
    a.printGraph
    println("\nprint contents of  map")
    for (v <- a.getGraph) {
      println(v._1 + ":" + a.adj(v._1))
    }
    
    println("\nshow connections of each vertex:")
       for (v <- a.getGraph) {
      println("connections for " + v._1 + ":" )
      val search = Search(a, v._1)
    }
  }
  
  /** TEST DFS algorithm **/
  def testDFS = {
        /** create a graph object **/
//    val a = initializeGraph("dummy.txt", GraphConstants.undirected)
//    val a = initializeGraph("numbers.txt", GraphConstants.undirected)
//    val a = initializeGraph("C:\\temp\\", "princetonMedium.txt", undirected)
      val a = initializeGraph(filename, undirected)
    println("\nmaxDegree of a is %d".format(maxDegree(a)))
    val showTrace = true
    if ( showTrace ) {
    println("\nprinting graph via printGraph")
    a.printGraph
    println("\nprint contents of  map")
    for (v <- a.getGraph) {
      println(v._1 + ":" + a.adj(v._1))
    }
    }
    val s = GraphVertex("Montclair NJ",100,true)
    val start = System.currentTimeMillis()
    val search = Search(a, s)
    val end = System.currentTimeMillis()
    println("elapsed:%d".format((end - start)))
    
    println("showing paths to " + s )
    println("\nshow paths of each vertex:")
    for (v <- a.getGraph) {
      
      if ( search.hasPathTo(v._1)) {
        print(s + " to " + v._1 + ":" )
        val path = search.pathTo(v._1)
//        println("searching path:" + path)
        for ( x <- path ) {
          if ( x == s ) 
            print(x)
            else 
              print("-" + x)
        }
        println()
      } 
//      println("finished...." )
    }
  }
  
  /** TEST Breadth First Search **/
  def testBFS = {
       /** create a graph object **/
//    val a = initializeGraph("dummy.txt", GraphConstants.undirected)
//    val a = initializeGraph("numbers.txt", GraphConstants.undirected)
//    val a = initializeGraph("C:\\temp\\", "princetonMedium.txt", undirected)
      val a = initializeGraph(filename, undirected)
    println("\nmaxDegree of a is %d".format(maxDegree(a)))
    val showTrace = true
    if ( showTrace ) {
    println("\nprinting graph via printGraph")
    a.printGraph
    println("\nprint contents of  map")
    for (v <- a.getGraph) {
      println(v._1 + ":" + a.adj(v._1))
    }
    }
    val s = GraphVertex("Montclair NJ",100,true)
    val start = System.currentTimeMillis()
    val search = BreadthFirstPaths(a, s)
    val end = System.currentTimeMillis()
    println("elapsed:%d".format((end - start)))
    
    println("showing paths to " + s )
    println("\nshow paths of each vertex:")
    for (v <- a.getGraph) {
      
      if ( search.hasPathTo(v._1)) {
        print(s + " to " + v._1 + ":" )
        val path = search.pathTo(v._1)
//        println("searching path:" + path)
        for ( x <- path ) {
          if ( x == s ) 
            print(x)
            else 
              print("-" + x)
        }
        println()
      } 
//      println("finished...." )
    }
  }
  /**
   * MAIN testing entrance point
   */
  def main(args: Array[String]) {
    testBFS
  }
}