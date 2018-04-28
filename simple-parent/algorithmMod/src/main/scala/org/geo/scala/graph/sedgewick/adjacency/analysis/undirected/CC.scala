package org.geo.scala.graph.sedgewick.adjacency.analysis.undirected

import scala.collection.mutable
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._

import org.geo.scala.graph.sedgewick.adjacency.Graph
import scala.collection.mutable.Queue

trait CC[T] {
  /** find all paths from source s **/
  def process(t: GraphConstants.Value): Unit
  def connected(v: T, w: T): Boolean // are v and w connected ?
  def count: Int // number of connected components
  def id(v: T): Option[Int] // component identifier for v, key in adj map
}

object CC {
  def apply[T](graph: Graph[T]): CC[T] =
    new CCImpl[T](graph)

  /** private implementation **/
  private class CCImpl[T](
    private val graph: Graph[T]) extends CC[T] {

    private var recurTyp: GraphConstants.Value = _
    println("instantiating DepthFirstPathsImpl")
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var marked: mutable.Map[T, Boolean] = _
    private var edgeTo: mutable.Map[T, T] = _

    private var idV: mutable.Map[T, Int] = _
    private var counter = 0

    var dcounter = 0
    /** used for tracing and debugging **/
    var start = 0l
    /** used for tracing and debugging **/
    /**
     *         private implementations
     */
    private def DEBUG = false

    /**
     *         private implementations
     */

    /**
     * Two versions of a depth first search of a graph used
     * to determine the path between a source and destination
     * vertex in a graph. This works with both undirected and
     * directed graphs. This version deviates from the classical
     * versions given in most texts in that it uses a HashMap to
     * store both vertices and adjacency lists.
     *
     * @see https://github.com/sidhartha11/scalastudy
     */

    /** RECURSIVE VERSION **/

    // @tailrec

    private def dfsRecursive(graph: Graph[T], v: T): Unit = {
      marked(v) = true
      /** count each connected vertex **/
      idV(v) = counter
      /** get the adjacent neighbors of v **/
      for (w <- graph.adjreverse(v)) {
        /** if this neigbor has not been seen yet **/
        if (!hasPathTo(w)) {
          /** check the adjacent neighors of w **/
          dfsRecursive(graph, w)
        }
      }
    }

    /** ITERATIVE VERSION **/
    private def dfs(graph: Graph[T], v: T): Unit = {
      val stack = mutable.Stack[T]()
      /** push the first element onto the stack **/
      stack.push(v)

      while (!stack.isEmpty) {
        val s = stack.pop()
        if (!hasPathTo(s)) {
       
          marked(s) = true
          idV(s) = counter
          /** get all the adjacent vertices of s **/
          /** push each one that is not marked onto the stack **/
          for (w <- graph.adj(s)) {
            if (!hasPathTo(w)) {
             // idV(w) = counter
              stack.push(w)
            }
          }
        }
      }
    }

    override def toString = "CC:" + counter

    /**
     *          public api implementation
     */

    /**
     * @return the number of verticies connect to input
     * source vertex
     */
    def count: Int = counter

    def hasPathTo(v: T): Boolean = {
      val t = marked get v
      if (t == None) false
      else t get
    }
    def connected(v: T, w: T): Boolean = {
      id(v) == id(w)
    }

    def id(v: T): Option[Int] = {
      idV get v
    }
    def process(recurType: GraphConstants.Value): Unit = {
      /** disallow input of unknown vetex **/
      /** map used to determine is a vertex is connected to the input vertex,s **/
      println("recurType:" + recurType)
      marked = new mutable.HashMap[T, Boolean]()
      edgeTo = new mutable.HashMap[T, T]()
      idV    = new mutable.HashMap[T,Int]()

      /** count of all vertices connected to input vertex s **/
      counter = 0
      /**
       * Constructor Processing
       */
      recurTyp = recurType
      var fun: (Graph[T],T) => Unit = null
      recurTyp match {
        case GraphConstants.recursive     => fun = dfsRecursive _
        case GraphConstants.non_recursive => fun = dfs _
      }
    /**
     * This is the call in the constructor of ConnectedComponentsImpl
     * Basically, this will traverse all of the verticies until all have
     * been visited. Dfs will be called on each "non-visited" vertex
     */
    for ((k,v) <- graph.getGraph) {
      if ( ! hasPathTo(k) ) {
        fun(graph,k)
        counter += 1
      }
    }
      /** debug code **/
      if (DEBUG) {
        for ((k, v) <- marked) {
          println(k + "," + v)
        }
        println(marked)
      }
    }
  }

}

object runner extends App {

  val graph = instantiateGraph[String, Int](",")(CITIES)
  val cc = CC(graph)
  cc.process(GraphConstants.non_recursive)
  println("%d components".format(cc.count))
  var components: mutable.Map[Int,Queue[String]] = 
    new mutable.HashMap[Int,Queue[String]]() 
  
  for ( i <- 0 until cc.count ) components(i) = Queue[String]()
  
  for ( g <- graph.getGraph.keySet ) {
    components( cc.id(g).get ).enqueue(g.name)
  }
  
  for ( i <- 0 until cc.count ) {
    for ( q <- components (i) ) {
      print(q + " ")
    }
    println
  } 
}