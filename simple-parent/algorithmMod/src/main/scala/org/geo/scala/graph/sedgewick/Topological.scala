package org.geo.scala.graph.sedgewick
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.sedgewick.adjacency.analysis.directed.DirectedCycle
import org.geo.scala.graph.sedgewick.adjacency.analysis.directed.DepthFirstOrder
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._

trait Topological[T] {
  def isDAG: Boolean
  def order: Iterable[T]
  def process(t: GraphConstants.Value): Unit // initialize internal structures

}

object Topological {
  /** companion object **/
    def apply[T](graph: Graph[T]): Topological[T] =
    new TopologicalImpl[T](graph)
  private class TopologicalImpl[T](
    private val graph: Graph[T]) extends Topological[T] {
    private var dfs: DepthFirstOrder[T] = _
    private var isdag = false
    def process(recurType: GraphConstants.Value): Unit = {
      /** disallow input of unknown vetex **/
      val g = DirectedCycle(graph)
      g.process(recurType)
      if (!g.hasCycle) {
        dfs = DepthFirstOrder(graph)
        dfs.process(recurType)
        isdag = true
      } else {
        println("cycle detected in adjacency list")
      }
    }

    def order: Iterable[T] = {
      if ( dfs == null ) {
        new scala.collection.mutable.ArrayBuffer[T]()
      } else {
        dfs.reversePostorder
      }
    }
    def isDAG: Boolean = isdag
  }
}

object TopologicalMain {
  import Topological._
  def main(args: Array[String] ) {
        val adj = instantiateGraph[String, Int](",")("jobs.txt", GraphConstants.directed)
        adj.printGraph
        val top = Topological(adj)
        top.process(GraphConstants.recursive)
        for ( v <- top.order ) {
          println("v = %s".format(v))
        }
  }
}