package org.geo.scala.graph.sedgewick

import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.analysis.DepthFirstPaths

object TestDepthFirstPaths {


  def DEBUG=false
  def test(tyP: GraphConstants.Value) {
    println("testing:%s".format(tyP))
    /** create an adjacency object **/
    val adj = instantiateGraph[String, Int](CITIES)
    adj.printGraph

    /** create an instance of DepthFirstSearch Object **/
    val dfs = DepthFirstPaths(adj)

    /** pick a vertex to analyze **/
    val vertex = GraphVertexGen("ZooLand Delaware", 100)

    /** process the vertex **/
    dfs.process(tyP)(vertex)

    /** check for paths to vertex against all neighbors of vertex **/
    // adj.getGraph.keys.filter(k => dfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))

    adj.getGraph.keys.filter(k => dfs.hasPathTo(k)) foreach (k =>
      {
        print(vertex.name + " to " + k.name + ": ")
        for (p <- dfs.pathTo(k, vertex)) {

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
    val dfs = DepthFirstPaths(adj)

    /** pick a vertex to analyze **/
    val vertex = GraphVertexGen(src, 100)

    /** process the vertex **/
    dfs.process(tyP)(vertex)

    /** check for paths to vertex against all neighbors of vertex **/
    // adj.getGraph.keys.filter(k => dfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))

    adj.getGraph.keys.filter(k => dfs.hasPathTo(k)) foreach (k =>
      {
        print(vertex.name + " to " + k.name + ": ")
        for (p <- dfs.pathTo(k, vertex)) {

          if (p == vertex)
            print(p.name)
          else
            print("-" + p.name)
        }
        println
      })

    println

  }
  
    def testTraceHop(src: String, file: String, tyP: GraphConstants.Value) = 
    {
    println("testing:%s".format(tyP))
    /** create an adjacency object **/
    val adj = instantiateGraph[String, Int](file)
    if (DEBUG)
    adj.printGraph

    /** create an instance of DepthFirstSearch Object **/
    val dfs = DepthFirstPaths(adj)

    /** pick a vertex to analyze **/
    val vertex = GraphVertexGen(src, 100)

    /** process the vertex **/
    dfs.process(tyP)(vertex)

    /** check for paths to vertex against all neighbors of vertex **/
    // adj.getGraph.keys.filter(k => dfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))
    var cntr=0
    var output:List[String] = List[String]()
    adj.getGraph.keys.filter(k => dfs.hasPathTo(k)) foreach (k =>
      {
        if (DEBUG)
        print(vertex.name + " to " + k.name + ": ")
        
        output =  output ::: List[String](vertex.name + " to " + k.name + ": " )
        for (p <- dfs.pathTo(k, vertex)) {

          if (p == vertex) {
            output =  output ::: List[String](p.name)
            if (DEBUG)
            print(p.name)
          }
          else {
            output =  output ::: List[String]("-" + p.name)
            cntr += 1
            if (DEBUG)
            print("-" + p.name)
          }
        }
        println("hops=%d, list=%s".format(cntr , output))
        cntr=0
        output = List[String]()
        if (DEBUG)
        println
      })
    if (DEBUG)
    println

 //   println("hops=%d, list=%s".format(cntr , output))
  }
    
    def testTraceHopList(src: String, file: String, tyP: GraphConstants.Value) = 
    {
    println("testing:%s".format(tyP))
    /** create an adjacency object **/
    var time = System.currentTimeMillis()
    val adj = instantiateGraph[String, Int](file)
    println("took %d seconds to read and load adj file".format(elapsed(time)))
    if (DEBUG)
    adj.printGraph
    println("size=" + adj.getGraph.size)

    /** create an instance of DepthFirstSearch Object **/
    println("creating DepthFirstPaths Structure")
    time = System.currentTimeMillis()
    val dfs = DepthFirstPaths(adj)
    println("took %d seconds to instantiate dfs".format(elapsed(time)))

    /** pick a vertex to analyze **/
    println("picking a vertex to check")
    val vertex = GraphVertexGen(src, 100)

    /** process the vertex **/
    println("processing vertex:" + vertex.name)
        time = System.currentTimeMillis()
    dfs.process(tyP)(vertex)
    println("took %d seconds to process dfs".format(elapsed(time)))

    println("finding paths")
    /** check for paths to vertex against all neighbors of vertex **/
    // adj.getGraph.keys.filter(k => dfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))
    var cntr=0
    var total=0
    var output:List[String] = List[String]()
    var collect = List[(Int,List[String])]()
    adj.getGraph.keys.filter(k => dfs.hasPathTo(k)) foreach (k =>
      {
        if ( total % 10000 == 0 ) println("so far:%d".format(total))
        total += 1
        //if (DEBUG)
        print(vertex.name + " to " + k.name + ": ")
        
        output =  output ::: List[String](vertex.name + " to " + k.name + ": " )
        for (p <- dfs.pathTo(k, vertex)) {

          if (p == vertex) {
            output =  output ::: List[String](p.name)
            if (DEBUG)
            print(p.name)
          }
          else {
            output =  output ::: List[String]("-" + p.name)
            cntr += 1
            if (DEBUG)
            print("-" + p.name)
          }
        }
        if ( DEBUG )
        println("hops=%d, list=%s".format(cntr , output))
        collect = collect ::: List[(Int,List[String])]((cntr,output))
//        println(collect)
        cntr=0
        output = List[String]()
        if (DEBUG)
        println
      })
    if (DEBUG)
    println
    // collect.foreach(l => println("cnt=%d\nlist=%s".format(l._1,l._2)))

 //   println("hops=%d, list=%s".format(cntr , output))
  }
    
    def testTraceHopListB(src: String, file: String, tyP: GraphConstants.Value) = 
    {
    println("testing:%s".format(tyP))
    /** create an adjacency object **/
    var time = System.currentTimeMillis()
    val adj = instantiateGraph[String, Int](file)
    println("took %d seconds to read and load adj file".format(elapsed(time)))
//    if (DEBUG)
//    adj.printGraph
    println("size=" + adj.getGraph.size)

    /** create an instance of DepthFirstSearch Object **/
    println("creating DepthFirstPaths Structure")
    time = System.currentTimeMillis()
    val dfs = DepthFirstPaths(adj)
    println("took %d seconds to instantiate dfs".format(elapsed(time)))

    /** pick a vertex to analyze **/
    println("picking a vertex to check")
    val vertex = GraphVertexGen(src, 100)

    /** process the vertex **/
    println("processing vertex:" + vertex.name)
        time = System.currentTimeMillis()
    dfs.process(tyP)(vertex)
    println("took %d seconds to process dfs".format(elapsed(time)))

    println("finding paths")
    /** check for paths to vertex against all neighbors of vertex **/
    // adj.getGraph.keys.filter(k => dfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))
    var cntr=0
    var total=0
    var output:List[String] = List[String]()
    adj.getGraph.keys foreach (k =>
      {
        if ( dfs.hasPathTo(k) ) {
        if ( total % 10000 == 0 ) println("so far:%d".format(total))
        total += 1
        // if (DEBUG)
        println(vertex.name + " to " + k.name + ": ")
        
        for (p <- dfs.pathTo(k, vertex)) {

          if (p == vertex) {
            if (DEBUG)
            println(p.name)
          }
          else {
            cntr += 1
            if (DEBUG)
            println("-" + p.name)
          }
        }
        println("hops=%d".format(cntr ))
        cntr=0
        if (DEBUG)
        println
        
      } 
//      else {
//        println("no path found for:" + k.name)
//      }
      
      })
    if (DEBUG)
    println
  }

  def main(args: Array[String]) {
    //   test(GraphConstants.non_recursive)
    //   test(GraphConstants.recursive)
    //    testTrace("ZooLand Delaware",CITIES,GraphConstants.recursive)
    // testTrace("0", TINYGC, GraphConstants.recursive)
    // testTrace("0", TINYGC, GraphConstants.non_recursive)
//    testTrace("ZooLand Delaware", CITIES, GraphConstants.non_recursive)
//    testTraceHop("ZooLand Delaware", CITIES, GraphConstants.non_recursive)
//    testTraceHop("999037", MEDIUMNUMBERS, GraphConstants.non_recursive)

    testTraceHopListB("999037", PRINCETONLARGE, GraphConstants.non_recursive)

  }
}