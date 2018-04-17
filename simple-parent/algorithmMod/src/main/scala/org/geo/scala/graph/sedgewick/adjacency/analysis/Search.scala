package org.geo.scala.graph.sedgewick.adjacency

import scala.collection.mutable
import java.util.Stack
import org.geo.scala.graph.sedgewick.GraphUtilities._

trait Search[T] {
  def count: Int // how many vertices are connected to s?
  def hasPathTo(v: T): Boolean // is v connected to a source vertex
  def pathTo(v: T): Iterable[T] // path from s to v
}

object Search {
  def apply[T](graph: Graph[T], s: T): Search[T] =
    new SearchImpl[T](graph, s)

  /** private implementation **/
  private class SearchImpl[T](
    private val graph: Graph[T],
    private val s:     T) extends Search[T] {
    require(requireVertexInGraph(graph,s),"vertex not in graph")
    private var edgeTo: mutable.Map[T, T] = new mutable.HashMap[T, T]()
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var mark: mutable.Map[T, Boolean] = new mutable.HashMap[T, Boolean]()
    /** count of all vertices connected to input vertex s **/
    var numberConnectedVertices = 0
    /**
     * This is the call in the constructor of SearchImpl
     */
    dfs(graph,s)
    for ( (k,v) <- mark ) {
      println(k + "," + v)
    }
    println(mark)
    /**
     * Following is the list of methods/functions in SearchImpl Implementation
     */
    def count: Int = numberConnectedVertices

    override def toString = "Search:" + s

    private def dfs(graph: Graph[T], v: T): Unit = {
      mark(v) = true
      for (w <- graph.adj(v)) {
        if (!hasPathTo(w)) {
          edgeTo(w) = v
          dfs(graph, w)
        }
      }
    }

    def hasPathTo(v: T): Boolean = {
      val t = mark get v
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
