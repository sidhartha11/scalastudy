package org.geo.scala.graph.sedgewick

import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.analysis.BreadthFirstPaths


object TestBreathFirstPaths {

  def test(tyP: GraphConstants.Value) {
    println("testing:%s".format(tyP))
    /** create an adjacency object **/
    val adj = instantiateGraph[String, Int](CITIES)
    adj.printGraph

    /** create an instance of DepthFirstSearch Object **/
    val bfs = BreadthFirstPaths(adj)

    /** pick a vertex to analyze **/
    val vertex = GraphVertexGen("ZooLand Delaware", 100)

    /** process the vertex **/
    bfs.process(tyP)(vertex)

    /** check for paths to vertex against all neighbors of vertex **/
    // adj.getGraph.keys.filter(k => bfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))

    adj.getGraph.keys.filter(k => bfs.hasPathTo(k)) foreach (k =>
      {
        print(vertex.name + " to " + k.name + ": ")
        for (p <- bfs.pathTo(k, vertex)) {

          if (p == vertex)
            print(p.name)
          else
            print("-" + p.name)
        }
        println
      })

    println

  }

  def testTrace(src: String, file: String, tyP: GraphConstants.Value) {
    println("testing:%s".format(tyP))
    /** create an adjacency object **/
    val adj = instantiateGraph[String, Int](file)
    adj.printGraph

    /** create an instance of DepthFirstSearch Object **/
    val bfs = BreadthFirstPaths(adj)

    /** pick a vertex to analyze **/
    val vertex = GraphVertexGen(src, 100)

    /** process the vertex **/
    bfs.process(tyP)(vertex)

    /** check for paths to vertex against all neighbors of vertex **/
    // adj.getGraph.keys.filter(k => bfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))

    adj.getGraph.keys.filter(k => bfs.hasPathTo(k)) foreach (k =>
      {
        print(vertex.name + " to " + k.name + ": ")
        for (p <- bfs.pathTo(k, vertex)) {

          if (p == vertex)
            print(p.name)
          else
            print("-" + p.name)
        }
        println
      })

    println

  }

  def main(args: Array[String]) {
    //   test(GraphConstants.non_recursive)
    //   test(GraphConstants.recursive)
    //    testTrace("ZooLand Delaware",CITIES,GraphConstants.recursive)
    testTrace("0", TINYGC, GraphConstants.non_recursive)
    testTrace("0", TINYGC, GraphConstants.recursive)
  }
}