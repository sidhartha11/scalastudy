package org.geo.scala.graph.sedgewick.adjacency.analysis

import scala.collection.mutable
import java.util.Stack
import org.geo.scala.graph.sedgewick.GraphUtilities._
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.GraphConstants

trait DepthFirstPaths[T] {
  /** find all paths from source s **/
  def process(t: GraphConstants.Value)(s: T): Unit 
  def hasPathTo(v: T): Boolean // is there a path from s to v?
  def pathTo(v: T,s: T): Iterable[T] /** path from s to v **/
}

object DepthFirstPaths {
  def apply[T](graph: Graph[T]): DepthFirstPaths[T] =
    new DepthFirstPathsImpl[T](graph)

  /** private implementation **/
  private class DepthFirstPathsImpl[T](
    private val graph: Graph[T]) extends DepthFirstPaths[T] {
    private var recurTyp: GraphConstants.Value = _
    println("instantiating DepthFirstPathsImpl")
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var marked: mutable.Map[T, Boolean] = _
    private var edgeTo: mutable.Map[T,T] = _

    /** count of all vertices connected to input vertex s **/
    var counter = 0

    /**
     *         private implementations
     */
    private def DEBUG = true
    private def dfsRecursive(graph: Graph[T], v: T): Unit = {
      marked(v) = true
      /** count each connected vertex **/
      counter += 1
      /** get the adjacent neighbors of v **/
      for (w <- graph.adj(v)) {
        /** if this neigbor has not been seen yet **/
        if (!hasPathTo(w)) {
          edgeTo(w) = v
          /** check the adjacent neighors of w **/
          dfsRecursive(graph, w)
        }
      }
    }

    private def dfs(graph: Graph[T], v: T): Unit = {
      val stack = mutable.Stack[T]()
      /** push the first element onto the stack **/
      stack.push(v)
      while (!stack.isEmpty) {
        val s = stack.pop()
        if ((marked get s) == None) {
          counter += 1
          marked(s) = true
        }

        /** get all the adjacent vertices of s **/
        /** push each one that is not marked onto the stack **/
        for (w <- graph.adj(s)) {
          if ((marked get w) == None) {
            edgeTo += (w -> v)
            stack.push(w)
          }
        }
      }
    }

    override def toString = "DepthFirstPaths:" + counter

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
      else true
    }
    
    def pathTo(v: T,s: T): Iterable[T] = {
      if (!hasPathTo(v)) {
        Iterable.empty[T]
      } else {
        var path = mutable.Buffer[T]()
        var x = v
        /** adding **/
        x +=: path
        while (x != s) {
          x = edgeTo(x)
          x +=: path
        }
        /** commented out **/
        // s +=: path
        path
      }
    }

    def process(recurType: GraphConstants.Value)(s: T): Unit = {
      /** disallow input of unknown vetex **/
      require(requireVertexInGraph(graph, s), "vertex not in graph")

      /** map used to determine is a vertex is connected to the input vertex,s **/
      marked = new mutable.HashMap[T, Boolean]()
      edgeTo = new mutable.HashMap[T, T]()

      /** count of all vertices connected to input vertex s **/
      counter = 0
      /**
       * Constructor Processing
       */
      recurTyp = recurType
      recurTyp match {
        case GraphConstants.recursive     => dfsRecursive(graph, s)
        case GraphConstants.non_recursive => dfs(graph, s)
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