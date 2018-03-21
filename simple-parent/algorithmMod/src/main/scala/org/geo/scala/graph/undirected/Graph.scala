package org.geo.scala.graph.undirected

import scala.collection.mutable
import scala.io.Source
import GraphUtilities._

trait Graph[T] {
  def addEdge(v: T, w: T) // add edge v-w to this graph
  def adj(v: T): List[T] // vertices adjacent to v
  def V: Int // number of vertices
  def E: Int // number edges
  def printGraph: Unit // print the contents of graph
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
    private var GraphMap: mutable.Map[T, mutable.Map[T, T]] = new mutable.HashMap[T, mutable.Map[T, T]]()

    def addEdge(u: T, v: T): Unit = {
      /** if biDir then must update both as neighbors **/
      biDir match {
        case GraphConstants.undirected => undirectedInsertion(u, v)
        case GraphConstants.directed   => directedInsertion(u, v)
      }
    }

    private def addMapping(u: T, v: T): Unit = {
      var t = GraphMap get u
      if (t == None) {
        val r = new mutable.HashMap[T, T]()
        r += (v -> v)
        GraphMap += (u -> r)
      } else {
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
      for ((key, list) <- GraphMap) {
        print("\nVertex|" + key + "| ")
        for ((e1, e2) <- list) {
          print(e2 + "->")
        }
        print("nil")
      }
    }

    def adj(v: T): List[T] = {
      return List()
    }

    def V: Int = numberVertices // number of vertices
    def E: Int = numberEdges // number edges

    override def toString = "Graph:" + GraphMap.mkString("[", ",", "]")
  }
}