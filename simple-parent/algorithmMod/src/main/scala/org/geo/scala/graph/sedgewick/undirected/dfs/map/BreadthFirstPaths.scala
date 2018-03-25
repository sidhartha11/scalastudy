package org.geo.scala.graph.sedgewick.undirected.dfs.map

import scala.collection.mutable
import GraphUtilities._

trait BreadthFirstPaths[T] {
  def count: Int // how many vertices are connected to s?
  def hasPathTo(v: T): Boolean // is v connected to a source vertex
  def pathTo(v: T): Iterable[T] // path from s to v
}

object BreadthFirstPaths {
  def apply[T](graph: Graph[T], s: T): BreadthFirstPaths[T] =
    new BreadthFirstPathsImpl[T](graph, s)

  /** private implementation **/
  private class BreadthFirstPathsImpl[T](
    private val graph: Graph[T],
    private val s:     T) extends BreadthFirstPaths[T] {
    require(requireVertexInGraph(graph, s), "vertex not in graph")
    private var edgeTo: mutable.Map[T, T] = new mutable.HashMap[T, T]()
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var marked: mutable.Map[T, Boolean] = new mutable.HashMap[T, Boolean]()
    /** count of all vertices connected to input vertex s **/
    var numberConnectedVertices = 0
    /**
     * This is the call in the constructor of BreadthFirstPathsImpl
     */
    bfs(graph, s)
    for ((k, v) <- marked) {
      println(k + "," + v)
    }

    /**
     * Following is the list of methods/functions in BreadthFirstPathsImpl Implementation
     */
    def count: Int = numberConnectedVertices

    override def toString = "BreadthFirstPaths:" + s

    private def bfs(graph: Graph[T], s: T): Unit = {
      var queue: mutable.Queue[T] = new mutable.Queue[T]()
      marked(s) = true
      queue += s
      while (!queue.isEmpty) {
        val v = queue.dequeue // remove vertex from queue
        for (w <- graph.adj(v)) {
          if (!hasPathTo(w)) { // for every unmarked adjacent vertex
            edgeTo(w) = v // save last edge on a shortest path
            marked(w) = true // mark it because path is known
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

    def pathTo(v: T): Iterable[T] = {
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
  }

}
