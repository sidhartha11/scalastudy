package org.geo.scala.graph.sedgewick.undirected.dfs.map

import scala.collection.mutable
import java.util.Stack
import GraphUtilities._

trait TwoColor[T] {
  def isBipartite: Boolean
  def count: Int // how many vertices are connected to s?
  def connected(v: T, w: T): Boolean   // are v and w connected
  def iD(v: T): Int    // return the id of the connected-component that v belongs to
}

object TwoColor {
  def apply[T](graph: Graph[T]): TwoColor[T] =
    new TwoColorImpl[T](graph)

  /** private implementation **/
  private class TwoColorImpl[T](
    private val graph: Graph[T]) extends TwoColor[T] {
   
    private var id: mutable.Map[T,Int] = new mutable.HashMap[T,Int]()
    private var edgeTo: mutable.Map[T, T] = new mutable.HashMap[T, T]()
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var marked: mutable.Map[T, Boolean] = new mutable.HashMap[T, Boolean]()
    /** color marked to determine Bipartitedness **/
    private var color: mutable.Map[T, Boolean] = new mutable.HashMap[T, Boolean]()
    /** count of all vertices connected to input vertex s **/
    var numberConnectedVertices = 0
    /** is a TwoColor present **/
    var isTwoColorable = true
   
    /**
     * This is the call in the constructor of TwoColorImpl
     * Basically, this will traverse all of the verticies until all have
     * been visited. Dfs will be called on each "non-visited" vertex
     * In addition, this code will determine if a TwoColor exist in the 
     * graph.
     */
    for ((k,v) <- graph.getGraph) {
      if ( ! hasPathTo(k) ) {
        dfs(graph,k)
        numberConnectedVertices += 1
      }
    }
    for ( (k,v) <- marked ) {
      println(k + "," + v)
    }
    /**
     * Following is the list of methods/functions in TwoColorImpl Implementation
     */
    def count: Int = numberConnectedVertices
    
    override def toString = "Cycle:" + numberConnectedVertices

    private def dfs(graph: Graph[T], v: T): Unit = {
      marked(v) = true
      id(v) = numberConnectedVertices
      for (w <- graph.adj(v)) {
        println("looking a %s for %s".format(w,v))
        if (!hasPathTo(w)) {
          edgeTo(w) = v
          color(w) = !isColored(v)
          dfs(graph, w)
        } else if (isColored(w) == isColored(v) ) {
          println("colors are equivalent:u=%s,v=%s".format(w,v))
          isTwoColorable = false
        }
      }
    }

    def hasPathTo(v: T): Boolean = {
      val t = marked get v
      if (t == None) false
      else true
    }
    
        def isColored(v: T): Boolean = {
      val t = color get v
      if (t == None) false
      else true
    }
    
    def connected(v: T, w: T): Boolean = {
      val v_id = id get v
      val w_id = id get w
      if ( (v_id == None) || (w_id == None ) )
          false
          
      val v_ida = v_id.get 
      val w_ida = w_id.get 
      return v_ida == w_ida
    }
    
    def iD(v: T): Int = {
      val v_id = id get v
      if ( v_id == None ) 
        return -1
      return v_id.get
    }
    
    def isBipartite: Boolean = {
      isTwoColorable
    }
  }
}