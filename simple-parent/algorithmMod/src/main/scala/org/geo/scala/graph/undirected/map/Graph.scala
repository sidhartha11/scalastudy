package org.geo.scala.graph.undirected.map

import scala.collection.mutable
import scala.io.Source
import GraphUtilities._

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
    private var graphMap: mutable.Map[T, mutable.Map[T, T]] = new mutable.HashMap[T, mutable.Map[T, T]]()

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

    private def directedInsertion(u: T, v: T): Unit = {
      /** directed map **/
      addMapping(u, v)
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
      println
    }
    
    def adj(v: T): Iterable[T] = {
      val a = graphMap get v
      a.get.values
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