package org.geo.scala.graph.sedgewick.undirected.dfs.buffer

import scala.collection.mutable
import java.util.Stack
import GraphUtilities._

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

    private var edgeTo: mutable.Map[T, T] = new mutable.HashMap[T, T]()
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var mark: mutable.Map[T, Boolean] = new mutable.HashMap[T, Boolean]()
    /** count of all vertices connected to input vertex s **/
    var numberConnectedVertices = 0
    /**
     * This is the call in the constructor of SearchImpl
     */
    require(requireVertexInGraph(graph, s), "vertex not in graph")
    println("\n...")
    //    var adJ = graph.adj(s)

    //    for (w <- adJ ) {
    //    println("looking for " + w)
    val exception =
      try {
        dfs(graph, s)
      } catch {
        case e1: java.lang.StackOverflowError => "Got a Stack Over Error:" + e1.getMessage
        case e => {
          e.printStackTrace
          "got an unknown type of error:" + e.getMessage
        }
      }
    if (exception != "true") {
      throw new RuntimeException("got error:" + exception)
    }
    for ((k, v) <- mark) {
      println(k + "," + v)
    }
    //        println("mark=" + mark)
    //    println("edgeTo=" + edgeTo)
    //    }

    /**
     * Following is the list of methods/functions in SearchImpl Implementation
     */
    def count: Int = numberConnectedVertices

    override def toString = "Search:" + s

    private def dfs(graph: Graph[T], v: T): String = {
      mark(v) = true
      for (w <- graph.adj(v)) {
        //        println("dfs in loop w=" + w)
        if (!hasPathTo(w)) {
          edgeTo(w) = v
          dfs(graph, w)
        }
      }
      return "true"
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
        /** adding, not in sedgewick **/
        x +=: path
        //        println("pathgen=" + path)
        while (x != s) {
          x = edgeTo(x)
          x +=: path
          //         println("pathgen=" + path)
        }
        /** commented out, not in sedgewick  **/
        //  s +=: path
        path
      }
    }
  }

}
