package org.geo.scala.graph.undirected

object GraphTester {
  def main(args: Array[String]) {
    val a: Graph[GraphVertex] = Graph(GraphConstants.undirected)
    println("a.V=%d,a.E=%d".format(a.V, a.E))
    for ( (node,neighbor,weight) <- GraphUtilities.readGraph("dummy.txt")) {
       a.addEdge(GraphVertex(node,weight,false),GraphVertex(neighbor,weight,false))
     }
    a.printGraph
  }
}