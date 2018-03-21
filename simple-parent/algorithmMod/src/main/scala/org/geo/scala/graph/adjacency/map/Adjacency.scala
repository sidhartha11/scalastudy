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
  def addEdge(u: T, v: T)
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

    def addEdge(u: T, v: T): Unit = {
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
  def testString = {
        println("testing")

    val node =  StringNode("bob",10,false)
    println("created node:" + node)
    val a: Adjacency[String] = Adjacency(MatricConstants.undirected)

    a.addEdge("A", "Z")
    a.addEdge("A", "B")
    a.addEdge("C", "A")
    a.addEdge("D", "E")
    a.addEdge("E", "D")
    a.addEdge("E", "A")
    a.addEdge("D", "Q")
    a.addEdge("D", "R")
    a.addEdge("D", "T")
    a.addEdge("X", "T")
    a.addEdge("X", "A")

    /** list the adjacency map **/
    a.printAdjacency
  }
  def main(args: Array[String]) {
    println("testing")

    val node =  StringNode("bob",10,false)
    println("created node:" + node)
    val a: Adjacency[StringNode] = Adjacency(MatricConstants.undirected)

    val data = 
   
    List(("A", "Z",10)
    ,("A", "B",20)
    ,("C", "A",20)
    ,("D", "E",5)
    ,("E", "D",6)
    ,("E", "A",90)
    ,("D", "Q",100)
    ,("D", "R",3)
    ,("D", "T",56)
    ,("X", "T",8)
    ,("X", "A",10)
     )
     
     for ( (node,neighbor,weight) <- data ) {
       a.addEdge(StringNode(node,weight,false),StringNode(neighbor,weight,false))
     }

    /** list the adjacency map **/
    a.printAdjacency
  }
}