package org.geo.scala.graph.sedgewick.adjacency.analysis

import scala.collection.mutable
import org.geo.scala.graph.sedgewick.GraphUtilities._
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.sedgewick.adjacency.Graph

/**
 * @author george
 * BFS takes time proportional to V+E in the worst case
 * For any vertex v reachable from s,
 * BFS computes a shortest path from s to v
 * We omit this part of the time complexity since this implementation
 * (no path from s to v has fewer edges).
 * uses Maps:
 * Initialzing the marked[] and edgeTo[] arrays takes time proportional to V.
 *
 *
 * @param <T>
 */
trait BreadthFirstPaths[T] {
  def process(t: GraphConstants.Value)(s: T): Unit
  def hasPathTo(v: T): Boolean // is v connected to a source vertex
  def pathTo(v: T, s: T): Iterable[T]
  /** path from s to v **/
}

object BreadthFirstPaths {
  def apply[T](graph: Graph[T]): BreadthFirstPaths[T] =
    new BreadthFirstPathsImpl[T](graph)

  /** private implementation **/
  private class BreadthFirstPathsImpl[T](
    private val graph: Graph[T]) extends BreadthFirstPaths[T] {

    private var edgeTo: mutable.Map[T, T] = new mutable.HashMap[T, T]()
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var marked: mutable.Map[T, Boolean] = new mutable.HashMap[T, Boolean]()

    /** private implementations **/

    private def DEBUG = true

    /**
     * Following is the list of methods/functions in BreadthFirstPathsImpl Implementation
     */

    override def toString = "BreadthFirstPaths"

    private def bfs(graph: Graph[T], s: T): Unit = {
      var queue: mutable.Queue[T] = new mutable.Queue[T]()
      marked(s) = true
      /** put s onto the queue **/
      queue += s
      while (!queue.isEmpty) {
        /** remove the head of the queue, least recently entered **/
        val v = queue.dequeue // remove vertex from queue
        /** get all adjacency entries for v and check for not being seen before **/
        for (w <- graph.adj(v)) {
          if (!hasPathTo(w)) { // for every unmarked adjacent vertex
            /** if w has not been seen, save it as the shortest path so far **/
            edgeTo(w) = v // save last edge on a shortest path
            marked(w) = true // mark it because path is known
            /** add w to the queue so that its adjacency list can be checked **/
            queue += w // add it to the queue
          }
        }
      }
    }

    def hasPathTo(v: T): Boolean = {
      val t = marked get v
      if (t == None) false
      else true
    }

    def pathTo(v: T, s: T): Iterable[T] = {
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

      /**
       * Constructor Processing
       */
      recurType match {
        case GraphConstants.recursive     => throw new IllegalArgumentException("only non-recursive")
        case GraphConstants.non_recursive => bfs(graph, s)
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
