package org.geo.scala.graph.sedgewick

import org.geo.scala.graph.sedgewick.adjacency.analysis.directed.KosarajuSharirSCC
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.GraphConstants
import scala.collection.mutable
import scala.collection.mutable.Queue




object TestKosarajuSharirSCC {
  def main(args: Array[String]) {
  val graph = instantiateGraph[String, Int](",")("tinyDG.txt",GraphConstants.directed)
  val cc = KosarajuSharirSCC(graph)
  cc.process(GraphConstants.recursive)
  println("%d components".format(cc.count))
  var components: mutable.Map[Int,Queue[String]] = 
    new mutable.HashMap[Int,Queue[String]]() 
  
  for ( i <- 0 until cc.count ) components(i) = Queue[String]()
  
  for ( g <- graph.getGraph.keySet ) {
    components( cc.id(g).get ).enqueue(g.name)
  }
  
  for ( i <- 0 until cc.count ) {
    for ( q <- components (i) ) {
      print(q + " ")
    }
    println
  } 
  }
}