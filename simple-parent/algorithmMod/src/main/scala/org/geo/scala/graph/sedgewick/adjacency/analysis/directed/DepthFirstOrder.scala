package org.geo.scala.graph.sedgewick.adjacency.analysis.directed

import scala.collection.mutable
import java.util.Stack
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.GraphConstants
import scala.collection.mutable.Queue

trait DepthFirstOrder[T] {

  def process(t: GraphConstants.Value): Unit // initialize internal structures
  def marked(v: T): Boolean // is v connected to a source vertex
  def preorder: Iterable[T] // collect vertices before recursive call
  def postorder: Iterable[T] // collect vertices after recursive call
  def reversePostorder: Iterable[T] // collect vertices after recursive call on stack
}

object DepthFirstOrder {
  def DEBUG = false
  def apply[T](graph: Graph[T]): DepthFirstOrder[T] =
    new DepthFirstOrderImpl[T](graph)

  /** private implementation **/
  private class DepthFirstOrderImpl[T](
    private val graph: Graph[T]) extends DepthFirstOrder[T] {
    println("instantiating DepthFirstOrderImpl")
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var mark: mutable.Map[T, Boolean] = _
    /**
     * for generating the topological sorts
     */
    private var pre: Queue[T] = _
    private var post: Queue[T] = _
    private var reversePost: java.util.Stack[T] = _
    /**
     *         private implementations
     */
    private def dfsRecursive(graph: Graph[T], v: T): Unit = {
      pre.enqueue(v)
      mark(v) = true
      if (DEBUG) {
        println("mark=%s".format(mark))
      }
      /** count each connected vertex **/
      /** get the adjacent neighbors of v **/
      for (w <- graph.adj(v)) {
        /** if this neigbor has not been seen yet **/
        if ((mark get w) == None) {
          /** check the adjacent neighors of w **/
          dfsRecursive(graph, w)
        }
      }
      /** make calls after the recursive call **/
      post.enqueue(v)
      reversePost.push(v)
    }

    private def dfs(graph: Graph[T], v: T): Unit = {
      val stack = mutable.Stack[T]()
      /** push the first element onto the stack **/
      stack.push(v)
      while (!stack.isEmpty) {
        val s = stack.pop()
        if ((mark get s) == None) {
          mark(s) = true
        }

        /** get all the adjacent vertices of s **/
        /** push each one that is not mark onto the stack **/
        for (w <- graph.adj(s)) {
          if ((mark get w) == None) {
            stack.push(w)
          }
        }
      }
    }

    override def toString = "DepthFirstOrder"

    /**
     *          public api implementation
     */

    def marked(v: T): Boolean = {
      val t = mark get v
      if (t == None) false
      else t get
    }

    /**
     * find vertices in graph that are reachable by elements in set
     * @param recurType
     * @param set
     */
    def process(recurType: GraphConstants.Value): Unit = {
      /** disallow input of unknown vetex **/

      /** map used to determine is a vertex is connected to the input vertex,s **/
      mark = new mutable.HashMap[T, Boolean]()
      /** queue and statcks used to collect the various traversal orderings **/
      pre = new mutable.Queue[T]()
      post = new mutable.Queue[T]()
      reversePost = new java.util.Stack[T]()

      for (s <- graph.getGraph.keySet if !marked(s)) {
        recurType match {
          case GraphConstants.recursive     => dfsRecursive(graph, s)
          case GraphConstants.non_recursive => dfs(graph, s)
        }
      }
      /** debug code **/
      if (DEBUG) {
        for ((k, v) <- mark) {
          println(k + "," + v)
        }
        println(mark)
      }
    }

    def preorder: Iterable[T] = {
      pre.toIterable
    }

    def postorder: Iterable[T] = {
      post.toIterable
    }

    def reversePostorder: Iterable[T] = {
      var itr: mutable.ArrayBuffer[T] = mutable.ArrayBuffer[T]()
      while (reversePost.isEmpty == false) {
        itr += reversePost.pop
      }
      itr
    }
  }

}