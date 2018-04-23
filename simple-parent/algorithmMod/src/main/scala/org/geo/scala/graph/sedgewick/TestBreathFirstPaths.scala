package org.geo.scala.graph.sedgewick

import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.analysis.BreadthFirstPaths

object TestBreathFirstPaths {

  def DEBUG = true
  def DEBUG_GRAPH = false
  def test(tyP: GraphConstants.Value) {
    println("testing:%s".format(tyP))
    /** create an adjacency object **/
    val adj = instantiateGraph[String, Int](",")(CITIES)
    if (DEBUG) {
      adj.printGraph
    }

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
    val adj = instantiateGraph[String, Int](",")(file)
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
  def testTraceHop(src: String, file: String, tyP: GraphConstants.Value) =
    {
      println("testing:%s".format(tyP))
      /** create an adjacency object **/
      val adj = instantiateGraph[String, Int](",")(file)
      if (DEBUG)
        adj.printGraph

      /** create an instance of DepthFirstSearch Object **/
      val bfs = BreadthFirstPaths(adj)

      /** pick a vertex to analyze **/
      val vertex = GraphVertexGen(src, 100)

      /** process the vertex **/
      bfs.process(tyP)(vertex)

      /** check for paths to vertex against all neighbors of vertex **/
      // adj.getGraph.keys.filter(k => bfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))
      var cntr = 0
      var output: List[String] = List[String]()
      adj.getGraph.keys.filter(k => bfs.hasPathTo(k)) foreach (k =>
        {
          if (DEBUG)
            print(vertex.name + " to " + k.name + ": ")

          output = output ::: List[String](vertex.name + " to " + k.name + ": ")
          for (p <- bfs.pathTo(k, vertex)) {

            if (p == vertex) {
              output = output ::: List[String](p.name)
              if (DEBUG)
                print(p.name)
            } else {
              output = output ::: List[String]("-" + p.name)
              cntr += 1
              if (DEBUG)
                print("-" + p.name)
            }
          }
          println("hops=%d, list=%s".format(cntr, output))
          cntr = 0
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
      val adj = instantiateGraph[String, Int](",")(file)
      println("took %d seconds to read and load adj file".format(elapsed(time)))
      if (DEBUG_GRAPH)
        adj.printGraph
      println("size=" + adj.getGraph.size)

      /** create an instance of DepthFirstSearch Object **/
      println("creating BreadthFirstPaths Structure")
      time = System.currentTimeMillis()
      val bfs = BreadthFirstPaths(adj)
      println("took %d seconds to instantiate bfs".format(elapsed(time)))

      /** pick a vertex to analyze **/
      println("picking a vertex to check")
      val vertex = GraphVertexGen(src, 100)

      /** process the vertex **/
      println("processing vertex:" + vertex.name)
      time = System.currentTimeMillis()
      bfs.process(tyP)(vertex)
      println("took %d seconds to process bfs".format(elapsed(time)))

      println("finding paths")
      /** check for paths to vertex against all neighbors of vertex **/
      // adj.getGraph.keys.filter(k => bfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))
      var cntr = 0
      var total = 0
      var output: List[String] = List[String]()
      var collect = List[(Int, List[String])]()
      adj.getGraph.keys.filter(k => bfs.hasPathTo(k)) foreach (k =>
        {
          if (total % 10000 == 0) println("so far:%d".format(total))
          total += 1
          //if (DEBUG)
          print(vertex.name + " to " + k.name + ": ")

          output = output ::: List[String](vertex.name + " to " + k.name + ": ")
          for (p <- bfs.pathTo(k, vertex)) {

            if (p == vertex) {
              output = output ::: List[String](p.name)
              if (DEBUG)
                print(p.name)
            } else {
              output = output ::: List[String]("-" + p.name)
              cntr += 1
              if (DEBUG)
                print("-" + p.name)
            }
          }
          if (DEBUG) {
            collect = collect ::: List[(Int, List[String])]((cntr, output))
            for ((hops, list) <- collect) {
              println("hops=%d\npath=%s".format(hops, list))
            }
          }
          //        println(collect)
          cntr = 0
          output = List[String]()
          if (DEBUG)
            println
        })
      if (DEBUG)
        println
      // collect.foreach(l => println("cnt=%d\nlist=%s".format(l._1,l._2)))

      //   println("hops=%d, list=%s".format(cntr , output))
    }

  def testTraceHopListB(delim: String)(src: String, file: String, tyP: GraphConstants.Value) =
    {
      println("testing:%s".format(tyP))
      /** create an adjacency object **/
      var time = System.currentTimeMillis()
      val adj = instantiateGraph[String, Int](delim)(file)
      println("took %d seconds to read and load adj file".format(elapsed(time)))
      //    if (DEBUG)
      //    adj.printGraph
      println("size=" + adj.getGraph.size)

      /** create an instance of DepthFirstSearch Object **/
      println("creating BreadthFirstPaths Structure")
      time = System.currentTimeMillis()
      val bfs = BreadthFirstPaths(adj)
      println("took %d seconds to instantiate bfs".format(elapsed(time)))

      /** pick a vertex to analyze **/
      println("picking a vertex to check")
      val vertex = GraphVertexGen(src, 100)

      /** process the vertex **/
      println("processing vertex:" + vertex.name)
      time = System.currentTimeMillis()
      bfs.process(tyP)(vertex)
      println("took %d seconds to process bfs".format(elapsed(time)))

      println("finding paths")
      /** check for paths to vertex against all neighbors of vertex **/
      // adj.getGraph.keys.filter(k => bfs.marked(k) ) foreach(k => println(k.name + " " + k.weight + " "))
      var cntr = 0
      var total = 0
      var output: List[String] = List[String]()
      adj.getGraph.keys foreach (k =>
        {
          if (bfs.hasPathTo(k)) {
            if (total % 10000 == 0) println("so far:%d".format(total))
            total += 1
            // if (DEBUG)
            println(vertex.name + " to " + k.name + ": ")

            for (p <- bfs.pathTo(k, vertex)) {

              if (p == vertex) {
                if (DEBUG)
                  println(p.name)
              } else {
                cntr += 1
                if (DEBUG)
                  println("-" + p.name)
              }
            }
            println("hops=%d".format(cntr))
            cntr = 0
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

  /** TEST TO SHOW PATH FROM ONE POINT TO ANOTHER POINT **/
  def testShowConnectionsTwoPoints(
    delim: String, // field delemiter
    src:   String, // beginning point
    dst:   String, // endpoint
    file:  String, // input file data
    tyP:   GraphConstants.Value // use recursion or not
  ) =
    {
      println("testing:%s".format(tyP))
      /** create an adjacency object **/
      var time = System.currentTimeMillis()
      var adj: Graph[GraphVertexGen[String, Int]] = null

      adj = instantiateGraph[String, Int](delim)(file)

      println("took %d seconds to read and load adj file".format(elapsed(time)))
      if (DEBUG_GRAPH)
        adj.printGraph
      println("size=" + adj.getGraph.size)

      /** create an instance of DepthFirstSearch Object **/
      println("creating BreadthFirstPaths Structure")
      time = System.currentTimeMillis()
      val bfs = BreadthFirstPaths(adj)
      println("took %d seconds to instantiate bfs".format(elapsed(time)))

      /** pick a vertex to analyze **/
      println("picking a vertex to check")
      val vertex = GraphVertexGen(src, 100)
      val destination = GraphVertexGen(dst, 100)

      /** process the vertex **/
      println("processing vertex:" + vertex.name + " to " + destination.name)
      time = System.currentTimeMillis()
      bfs.process(tyP)(vertex)
      println("took %d seconds to process bfs".format(elapsed(time)))

      println("finding paths")
      /** check for paths to vertex against all neighbors of vertex **/
      var cntr = 0
      var total = 0
      var output: List[String] = List[String]()
      var collect = List[(Int, List[String])]()

      if (bfs.hasPathTo(destination)) {
        if (total % 10000 == 0) println("so far:%d".format(total))
        total += 1
        //if (DEBUG)
        println(vertex.name + " to " + destination.name + ": ")

        output = output ::: List[String](vertex.name + " to " + destination.name + ": ")
        for (p <- bfs.pathTo(destination, vertex)) {

          if (p == vertex) {
            output = output ::: List[String](p.name)
            //              if (DEBUG)
            //                print(p.name)
          } else {
            output = output ::: List[String]("-" + p.name)
            cntr += 1
            //              if (DEBUG)
            //                print("-" + p.name)
          }
        }
        if (DEBUG) {
          collect = collect ::: List[(Int, List[String])]((cntr, output))
          for ((hops, list) <- collect) {
            println("hops=%d\npath=%s".format(hops, list))
          }
        }
        //        println(collect)
        cntr = 0
        output = List[String]()
        if (DEBUG)
          println

      }

      if (DEBUG)
        (println)
    }
  def getStdin(recursive: GraphConstants.Value) = {
    val filename = scala.io.StdIn.readLine("%s", "enter filename")
    val delem = scala.io.StdIn.readLine("%s", "enter dilimeter")
    val source = scala.io.StdIn.readLine("%s", "enter source")
    val destination = scala.io.StdIn.readLine("%s", "enter destination")
    val weight = scala.io.StdIn.readLine("%s", "enter weight")
    val weightInt = Integer.parseInt(weight)
    println("entered:file=%s,src=%s,dst=%s,wt=%d".format(filename, source, destination, weightInt))

    testShowConnectionsTwoPoints(
      delem, // field dilememter
      source, // beginning point
      destination, // endpoint
      filename, // input file data
      recursive // use recursion or not

    )
  }

  def getHardCoded(recursive: GraphConstants.Value) = {
    val filename = "syntheticCities.txt"
    val delem = "|"
    val source = "Springfield, West Virginia"
    val destination = "Oxford, Maine"
    val weightInt = 127
    //    val filename = scala.io.StdIn.readLine("%s","enter filename")
    //    val delem = scala.io.StdIn.readLine("%s","enter dilimeter")
    //    val source = scala.io.StdIn.readLine("%s", "enter source")
    //    val destination = scala.io.StdIn.readLine("%s", "enter destination")
    //    val weight = scala.io.StdIn.readLine("%s", "enter weight")
    //    val weightInt = Integer.parseInt(weight)
    println("entered:file=%s,src=%s,dst=%s,wt=%d".format(filename, source, destination, weightInt))

    testShowConnectionsTwoPoints(
      delem, // field dilememter
      source, // beginning point
      destination, // endpoint
      filename, // input file data
      recursive // use recursion or not

    )
  }
  def main(args: Array[String]) {
    //   test(GraphConstants.non_recursive)
    //   test(GraphConstants.recursive)
    //   testTrace("ZooLand Delaware",CITIES,GraphConstants.recursive)
    //   testTrace("0", TINYGC, GraphConstants.non_recursive)
    //   testTrace("0", TINYGC, GraphConstants.recursive)
    //   testTrace("ZooLand Delaware", CITIES, GraphConstants.non_recursive)
    testTrace("ZooLand Delaware", CITIES, GraphConstants.non_recursive)   
    
    /** dumps adjacency map , volumous data output **/    
    // testTraceHop("999037", MEDIUMNUMBERS, GraphConstants.non_recursive)

    /** test showing connections and number of hops for all vertices **/
    // testTraceHopListB(",")("ZooLand Delaware", CITIES, GraphConstants.non_recursive)
    // testTraceHopListB("|")("Washington, Kansas", SYNTHETICCITIES, GraphConstants.non_recursive)
    
    
    /** test showing connections and number of hops for all vertices **/
    // testTraceHopListB(",")("999037", MEDIUMNUMBERS, GraphConstants.non_recursive)
    
    /** test using user console input **/
    //    getStdin( GraphConstants.non_recursive)
    /** test some hardcoded values against simulated data **/
    //    getHardCoded(GraphConstants.non_recursive)

  }
}