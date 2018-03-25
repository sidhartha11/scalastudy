package org.geo.scala.graph.sedgewick.undirected.dfs.map

import scala.collection.mutable
import scala.io.Source
import GraphUtilities._
import org.geo.scala.graph.GlobalUtilities._
import org.geo.scala.graph.GraphConstants

trait Graph[T] {
  def addEdge(v: T, w: T) // add edge v-w to this graph
  def adj(v: T): Iterable[T] // vertices adjacent to v
  def V: Int // number of vertices
  def E: Int // number edges
  def printGraph: Unit // print the contents of graph
  def getGraph: mutable.Map[T, mutable.Map[T, T]]
}

object Graph {
  def apply[T](biDir: GraphConstants.Value): Graph[T] =
    new GraphImpl[T](biDir)


  /** private implementation **/
  private class GraphImpl[T](
    private val biDir: GraphConstants.Value) extends Graph[T] {

    /** number of vertices in the graph **/
    private[this] var numberVertices = 0
    private[this] var numberEdges = 0
    /** create an empty map of maps to store the vertices and edges **/
    private var graphMap: mutable.Map[T, mutable.Map[T, T]] = new mutable.LinkedHashMap[T, mutable.Map[T, T]]()

    /** DISPLAY DIRECTION **/
    println("mapped adj " + biDir )
    
    def addEdge(u: T, v: T): Unit = {
      /** if biDir then must update both as neighbors **/
      biDir match {
        case GraphConstants.undirected => undirectedInsertion(u, v)
        case GraphConstants.directed   => directedInsertion(u, v)
      }
    }

    private def addMapping(u: T, v: T): Unit = {
      var t = graphMap get u
      if (t == None) {
        numberVertices += 1
        numberEdges += 1
        val r = new mutable.HashMap[T, T]()
        r += (v -> v)
        graphMap += (u -> r)
      } else {
        numberEdges += 1
        t.get += (v -> v)
      }
    }
    private def addMappingDirected(u: T, v: T): Unit = {
      var t = graphMap get u
      if (t == None) {
        /** update number of vertices and edges since this vertex does not exist **/
        numberVertices += 1
        numberEdges += 1
        /** create an entry for the adj list **/
        val r = new mutable.HashMap[T, T]()
        /** add the neighbor, v to the adj list **/
        r += (v -> v)
        //        r +=: v
        /** update the vertex object **/
        graphMap += (u -> r)
      } else {
        /** vertex is already present, so only count the edges **/
        numberEdges += 1
        //        t.get += (v -> v)
        /** add the new neighor to the adj map, note duplicates can not happen here **/
        t.get += (v -> v)
      }

      /** now check to see if the neighbor, v, exists in the vertex map only, if not **/
      /** put an entry there with an empty adj list for reference **/
      t = graphMap get v
      if (t == None) {
        /** only update the number of vertexies , since this is a on directional mapping only **/
        numberVertices += 1
        val r = mutable.HashMap[T,T]()
        /** put an empty place holder for the one directional vertex **/
        graphMap += (v -> r)
      }
    }
    private def directedInsertion(u: T, v: T): Unit = {
      /** directed map **/
      addMappingDirected(u, v)
    }

    private def undirectedInsertion(u: T, v: T): Unit = {
      /** get the first mapping **/
      addMapping(u, v)
      /** get the second side **/
      addMapping(v, u)
    }

    def printGraph = {
      numberEdges = 0
      numberVertices = 0
      for ((key, list) <- graphMap) {
        numberVertices += 1
        print("\nVertex|" + key + "| ")
        for ((e1, e2) <- list) {
          numberEdges += 1
          print(e2 + "->")
        }
        print("nil")
      }
      println("\n...")
    }
    
    def adj(v: T): Iterable[T] = {
      val a = graphMap get v
      if ( a == None ) {
        Iterable.empty[T]
      } else {
      a.get.values
      }
    }

    /**
     * returns the number of vertices 
     * @see org.geo.scala.graph.undirected.Graph#V()
     */
    def V: Int = numberVertices
 
    /**
     * returns the number of edges
     * @see org.geo.scala.graph.undirected.Graph#E()
     */
    def E: Int =  {
        biDir match {
        case GraphConstants.undirected => numberEdges / 2 
        case GraphConstants.directed   => numberEdges
      }
    }

    override def toString = "Graph:" + graphMap.mkString("[", ",", "]")

    def getGraph: mutable.Map[T, mutable.Map[T, T]] = graphMap
  }
}