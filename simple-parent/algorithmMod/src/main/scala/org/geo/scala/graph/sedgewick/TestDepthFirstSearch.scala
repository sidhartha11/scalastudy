package org.geo.scala.graph.sedgewick

import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.analysis.DepthFirstSearch

object TestDepthFirstSearch {

  def test(tyP: GraphConstants.Value) {
    println("testing:%s".format(tyP))
        /** create an adjacency object **/
    val adj = instantiateGraph[String,Int](NUMBERS)
    outPut(adj.toString())
    
    /** create an instance of DepthFirstSearch Object **/
    val dfs = DepthFirstSearch(adj)
    
    /** pick a vertex to analyze **/
    val vertex = GraphVertexGen("0",100)
    
    /** process the vertex **/
    dfs.process(tyP)(vertex)
    
    /** check each vertex for connectivity **/
    adj.getGraph.keys.filter(k => dfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))
    
    /** check number of connected nodes **/
    val b = (dfs count)  == adj.V
    println( if ( b ) "connected" else "not connected" )
  }
  
  def main(args: Array[String]) {
    test(GraphConstants.non_recursive)
    test(GraphConstants.recursive)
  }
}