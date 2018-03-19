package org.geo.scala.graph.adjacency.map

import scala.collection.mutable

/**
 * @author george curington
 * Simple implementation of an adjacency structure to be used later
 * in various graph algorithms.
 * So Far:
 * This simple implementation creates a an Adjacency List to store the
 * neighbors of each vertex that is analyzed. Normally, an array of vertices
 * is used to store the vertices with each location pointing the a linked list
 * which is used to store the neighbors of a node. The graph can be either
 * directed or undirected.
 * In this case I decided to , out of curiousity , use a map for both the vertices
 * and for the  edges.  I guess it is possible to use something more interesting than
 * a map; maybe a BST, or a Heap.
 * So the data structure I am using is:
 * <pre>
 * private var adjacencyMap: mutable.Map[T, mutable.Map[T, T]] = new mutable.HashMap[T, mutable.Map[T, T]]()
 * </pre>
 *
 */

object MatricConstants extends Enumeration {
  val directed = Value("directed graph")
  val undirected = Value("undirected graph")
  val adj_linked = Value("Using Linked Allocation")
  val adj_map = Value("Using Mapped Allocation")
  val adj_unim = Value("Feature unimplemented")
}

trait Adjacency[T] {
  def neighbor(u: T, v: T)
  def printAdjacency
}

object Adjacency {
  def apply[T](biDir: MatricConstants.Value): Adjacency[T] =
    new AdjacencyImpl[T](biDir)

  /** private implementation **/
  private class AdjacencyImpl[T](
    private val biDir: MatricConstants.Value) extends Adjacency[T] {

    /** create an empty map of maps to store the vertices and edges **/
    private var adjacencyMap: mutable.Map[T, mutable.Map[T, T]] = new mutable.HashMap[T, mutable.Map[T, T]]()

    def neighbor(u: T, v: T): Unit = {
      /** if biDir then must update both as neighbors **/
      biDir match {
        case MatricConstants.undirected => undirectedInsertion(u, v)
        case MatricConstants.directed   => directedInsertion(u, v)
      }
    }

    private def addMapping(u: T, v: T): Unit = {
      var t = adjacencyMap get u
      if (t == None) {
        val r = new mutable.HashMap[T, T]()
        r += (v -> v)
        adjacencyMap += (u -> r)
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

    def printAdjacency = {
      for ((key, list) <- adjacencyMap) {
        print("\nVertex|" + key + "| ")
        for ((e1, e2) <- list) {
          print(e2 + "->")
        }
        print("nil")
      }
    }
    override def toString = "Adjacency:" + adjacencyMap.mkString("[", ",", "]")
  }
}

object runnerTest {
  def main(args: Array[String]) {
    println("testing")

    val a: Adjacency[String] = Adjacency(MatricConstants.undirected)

    a.neighbor("A", "Z")
    a.neighbor("A", "B")
    a.neighbor("C", "A")
    a.neighbor("D", "E")
    a.neighbor("E", "D")
    a.neighbor("E", "A")
    a.neighbor("D", "Q")
    a.neighbor("D", "R")
    a.neighbor("D", "T")
    a.neighbor("X", "T")
    a.neighbor("X", "A")

    /** list the adjacency map **/
    a.printAdjacency
  }
}