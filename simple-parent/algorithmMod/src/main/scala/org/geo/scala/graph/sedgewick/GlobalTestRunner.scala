package org.geo.scala.graph.sedgewick

import org.geo.scala.graph.sedgewick.undirected.dfs.{ buffer => BUF }
import org.geo.scala.graph.sedgewick.undirected.dfs.{ map => MAP }

import org.geo.scala.graph._
import scala.collection.mutable

/**
 * @author George Curington
 * This object simply runs a test for both types
 * of adjacency graph implementations:
 * 1. Buffer used as an adjacency list
 * In this case the vertex map is still a "HashMap" but
 * the actual neighbor list is a linked list structure.
 * 
 * 2. HashMap used as an adjacency list
 * In this case the vertex map is still a "HashMap" but the 
 * actual neighbor list is a HashMap also. 
 * 
 * The type of test made is discriminated by using the import aliasing mechanism
 * above: BUF for buffer and MAP for HashMap
 * 
 * The source of input comes from a file on the file system, though it is possible 
 * to read from the net:
 * @see org.geo.scala.graph.GlobalUtilities.downloadFile
 * 
 * DIRECTION:
 * The adjacency list(or map) can be either directed or undirected.
 * THis is controlled by a variable to the instantiation of the Graph object.
 * @see org.geo.scala.graph.GraphConstants
 * There is an enumeration in GraphConstants that is used to determine the directedness:
 * undirected or directed.
 * e.g.
 * An instantiation of the Graph Object is as follows:
 * val a = BUF.GraphUtilities.initializeGraph(GlobalUtilities.filename, GraphConstants.undirected)
 * So here, filename will contain the input data in the form of:
 * node1,node2,weight where node1 and node2 are two connected verticies and weight is an 
 * indicator of the cost of the edge between them. ( currently not being used in this implementation )
 * 
 * Two types of search are implemented:
 * 1. recursive DFS search
 * This search type is limited by the stack size and will throw an Error is the input exceeds
 * available stack space.
 * 
 * 2. non-recursive BFS search 
 * 
 * 
 * 
 */
object GlobalTestRunner {

  /**
   * Test Files Used
   *  princetonLarge ( over 7meg ) 
   *  princetonMedium.txt ( 400K records )
   *  city.txt  small file of different cities 
   */
  def direction = GraphConstants.undirected
  // def nodeToAnalyze="Montclair NJ"
  def nodeToAnalyze="997847"
  def showTrace = true
  /** BUFFER BASED **/
  
    /** TEST DFS algorithm **/
  def testConnectedBUForMAP = {
    /** create a graph object **/
//  val a = BUF.GraphUtilities.initializeGraph(GlobalUtilities.filename, direction)
    val a = MAP.GraphUtilities.initializeGraph(GlobalUtilities.filename, direction)
    println("\nmaxDegree of a is %d".format(MAP.GraphUtilities.maxDegree(a)))
    if (showTrace) {
      println("\nprinting graph via printGraph")
      a.printGraph
      println("\nprint contents of  map")
      for (v <- a.getGraph) {
        println(v._1 + ":" + a.adj(v._1))
      }
    }

    println("running connected components test")
    val start = System.currentTimeMillis()
    
    val cc = MAP.ConnectedComponents(a)
//  val cc = BUF.ConnectedComponents(a)
    /** get the number of connected components **/
    val m = cc.count
    /**
     * Create a map of Int,Queue associations where each Int
     * is a pointer to a Queue of vertices representing 
     * connected components
     */
    val components = mutable.Map[Int,mutable.Queue[GraphVertex]]()
    
    /**
     * traverse thru all known vertices, locate the Integer
     * representing their connected components id,
     * if the queue for the component is not currently in the map,
     * put it there.
     * put the current vertex into the queue for that component,
     */
    
    for ( (v,l) <- a.getGraph ) {
      val comp = components get (cc.iD(v))
      if ( comp == None ) {
        val queue: mutable.Queue[GraphVertex] = new mutable.Queue[GraphVertex]()
        queue += v
        components += (cc.iD(v) -> queue)
      } else {
        comp.get += v
      }
    }
    
    /** scan thru the components Map and print out the contents **/
    for ( (c_id , q ) <- components ) {
      print("component id:" + c_id + "\n    ")
      for ( queue_item <- q ) {
        print(queue_item + " : ")
      }
      println()
    }
    
    val end = System.currentTimeMillis()
    println("elapsed:%d".format((end - start)))


  }
  /** TEST DFS algorithm **/
  def testDFSBUF = {
    /** create a graph object **/
    val a = BUF.GraphUtilities.initializeGraph(GlobalUtilities.filename, direction)
    println("\nmaxDegree of a is %d".format(BUF.GraphUtilities.maxDegree(a)))
    if (showTrace) {
      println("\nprinting graph via printGraph")
      a.printGraph
      println("\nprint contents of  map")
      for (v <- a.getGraph) {
        println(v._1 + ":" + a.adj(v._1))
      }
    }
    val s = GraphVertex(nodeToAnalyze, 100, true)
    val start = System.currentTimeMillis()
    val search = BUF.Search(a, s)
    val end = System.currentTimeMillis()
    println("elapsed:%d".format((end - start)))

    println("showing paths to " + s)
    println("\nshow paths of each vertex:")
    for (v <- a.getGraph) {

      if (search.hasPathTo(v._1)) {
        print(s + " to " + v._1 + ":")
        val path = search.pathTo(v._1)
        //        println("searching path:" + path)
        for (x <- path) {
          if (x == s)
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
  def testBFSBUF = {
    /** create a graph object **/
    val a = BUF.GraphUtilities.initializeGraph(GlobalUtilities.filename, direction)
    println("\nmaxDegree of a is %d".format(BUF.GraphUtilities.maxDegree(a)))
    if (showTrace) {
      println("\nprinting graph via printGraph")
      a.printGraph
      println("\nprint contents of  map")
      for (v <- a.getGraph) {
        println(v._1 + ":" + a.adj(v._1))
      }
    }
    val s = GraphVertex(nodeToAnalyze, 100, true)
    val start = System.currentTimeMillis()
    val search = BUF.BreadthFirstPaths(a, s)
    val end = System.currentTimeMillis()
    println("elapsed:%d".format((end - start)))

    println("showing paths to " + s)
    println("\nshow paths of each vertex:")
    for (v <- a.getGraph) {

      if (search.hasPathTo(v._1)) {
        print(s + " to " + v._1 + ":")
        val path = search.pathTo(v._1)
        //        println("searching path:" + path)
        for (x <- path) {
          if (x == s)
            print(x)
          else
            print("-" + x)
        }
        println()
      }
    }
  }
  
    /** MAPPED BASED **/
    /** TEST DFS algorithm **/
  def testDFSMAP = {
    /** create a graph object **/
    val a = MAP.GraphUtilities.initializeGraph(GlobalUtilities.filename, direction)
    println("\nmaxDegree of a is %d".format(MAP.GraphUtilities.maxDegree(a)))
    if (showTrace) {
      println("\nprinting graph via printGraph")
      a.printGraph
      println("\nprint contents of  map")
      for (v <- a.getGraph) {
        println(v._1 + ":" + a.adj(v._1))
      }
    }
    val s = GraphVertex(nodeToAnalyze, 100, true)
    val start = System.currentTimeMillis()
    val search = MAP.Search(a, s)
    val end = System.currentTimeMillis()
    println("elapsed:%d".format((end - start)))

    println("showing paths to " + s)
    println("\nshow paths of each vertex:")
    for (v <- a.getGraph) {

      if (search.hasPathTo(v._1)) {
        print(s + " to " + v._1 + ":")
        val path = search.pathTo(v._1)
        //        println("searching path:" + path)
        for (x <- path) {
          if (x == s)
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
  def testBFSMAP = {
    /** create a graph object **/
    val a = MAP.GraphUtilities.initializeGraph(GlobalUtilities.filename, direction)
    println("\nmaxDegree of a is %d".format(MAP.GraphUtilities.maxDegree(a)))
    if (showTrace) {
      println("\nprinting graph via printGraph")
      a.printGraph
      println("\nprint contents of  map")
      for (v <- a.getGraph) {
        println(v._1 + ":" + a.adj(v._1))
      }
    }
    val s = GraphVertex(nodeToAnalyze, 100, true)
    val start = System.currentTimeMillis()
    val search = MAP.BreadthFirstPaths(a, s)
    val end = System.currentTimeMillis()
    println("elapsed:%d".format((end - start)))

    println("showing paths to " + s)
    println("\nshow paths of each vertex:")
    for (v <- a.getGraph) {

      if (search.hasPathTo(v._1)) {
        print(s + " to " + v._1 + ":")
        val path = search.pathTo(v._1)
        //        println("searching path:" + path)
        for (x <- path) {
          if (x == s)
            print(x)
          else
            print("-" + x)
        }
        println()
      }
    }
  }
  
    def testBFSMAPLarge(filename: String)  = {
    /** create a graph object **/
    val a = MAP.GraphUtilities.initializeGraph("C:\\temp\\", filename, direction)
    println("\nmaxDegree of a is %d".format(MAP.GraphUtilities.maxDegree(a)))
    if (showTrace) {
      println("\nprinting graph via printGraph")
      a.printGraph
      println("\nprint contents of  map")
      for (v <- a.getGraph) {
        println(v._1 + ":" + a.adj(v._1))
      }
    }
    val s = GraphVertex(nodeToAnalyze, 100, true)
    val start = System.currentTimeMillis()
    val search = MAP.BreadthFirstPaths(a, s)
    val end = System.currentTimeMillis()
    println("elapsed:%d".format((end - start)))

    println("showing paths to " + s)
    println("\nshow paths of each vertex:")
    for (v <- a.getGraph) {

      if (search.hasPathTo(v._1)) {
        print(s + " to " + v._1 + ":")
        val path = search.pathTo(v._1)
        //        println("searching path:" + path)
        for (x <- path) {
          if (x == s)
            print(x)
          else
            print("-" + x)
        }
        println()
      }
    }
  }
  /**
   * MAIN testing entrance point
   */
  def main(args: Array[String]) {
    // testBUFimplementation
    // testBFSBUF
    // testBFSMAP
    // testDFSBUF
    // testBFSMAPLarge("princetonMedium.txt")
    testConnectedBUForMAP
  }
}