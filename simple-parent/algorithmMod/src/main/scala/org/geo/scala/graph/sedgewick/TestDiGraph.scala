package org.geo.scala.graph.sedgewick

import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.analysis.undirected.BreadthFirstPaths
import org.geo.scala.graph.sedgewick.adjacency.analysis.undirected.DepthFirstPaths
import org.geo.scala.graph.sedgewick.adjacency.analysis.directed.DirectedDFS
import org.geo.scala.graph.sedgewick.adjacency.analysis.directed.DirectedCycle

/**
 * @author george
 * This object is used for testing digraphs
 *
 */
object TestDiGraph {

  def DEBUG = true
  def DEBUG_GRAPH = false
  def test(tyP: GraphConstants.Value) {
    println("testing:%s".format(tyP))
    /** create an adjacency object **/
    val adj = instantiateGraph[String, Int](",")("tinyDG.txt", GraphConstants.directed)
    if (DEBUG) {
      adj.printGraph
    }

    /** now reverse the directed graph **/
    val adjR = adj.reverseGraph
    if (DEBUG) {
      println("showing reversed graph\n\n")
      adjR.printGraph
    }
  }

  def testDirectedDFS(args: Array[String]) {
    /** first create a directed adjacency graph **/
    val adj = instantiateGraph[String, Int](",")("tinyDG.txt", GraphConstants.directed)
    /** create a DirectedDFS instance **/
    val g = DirectedDFS(adj)
    /** create a list of Iterables to process **/
    val itr =
      for (i <- List("1", "2", "6").toIterable) yield {
        GraphVertexGen[String, Int](i, 100)
      }

    /** process the Iterables **/
    g.process(GraphConstants.recursive, true)(itr)

    /** now traverse thru the vertices to find out what is reachable **/
    /** by the list of Iterable input vertices **/
    for (k <- adj.getGraph.keySet if g.marked(k)) {
      print(k + " ")
    }
    println

  }

  def testDirectedCycle {
    /** first create a directed adjacency graph **/
    val adj = instantiateGraph[String, Int]("|")("syntheticCities.txt", GraphConstants.directed)
    
        if (DEBUG) {
      adj.printGraph
    }
    /** create a DirectedDFS instance **/
    val g = DirectedCycle(adj)

    g.process(GraphConstants.recursive)

    /** now traverse thru the vertices to find out what is reachable **/
    /** by the list of Iterable input vertices **/
    for (k <- adj.getGraph.keySet) {
      print(k + " ")
    }
    println

    /**
     * now check for cycles
     */
    if (g.hasCycle) {
      println("cycles detected")
      for (i <- g.cycle) {
        print(i + " ")
      }
      println
    } else {
      println("no cycles detected")
    }

  }

  /** TEST TO SHOW PATH FROM ONE POINT TO ANOTHER POINT **/
  def testShowConnectionsTwoPointsAdjBfs(
    adj: Graph[GraphVertexGen[String, Int]], // adjacency map
    src: String, // endpoint
    dst: String, // endpoint
    tyP: GraphConstants.Value // use recursion or not
  ) =
    {
      println("testing:%s".format(tyP))
      var time = System.currentTimeMillis()
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
        println(total + ":" + vertex.name + " to " + destination.name + ": ")

        output = output ::: List[String](vertex.name + " to " + destination.name + ": ")
        time = System.currentTimeMillis()

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
          println("took %d seconds to collect paths".format(elapsed(time)))

          collect = collect ::: List[(Int, List[String])]((cntr, output))
          for ((hops, list) <- collect) {
            list.take(25).foreach(println(_))
            println("hops=%d".format(hops))
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

  def testShowConnectionsTwoPointsAdjDfs(
    adj: Graph[GraphVertexGen[String, Int]], // adjacency map
    src: String, // endpoint
    dst: String, // endpoint
    tyP: GraphConstants.Value // use recursion or not
  ) =
    {
      println("testing:%s".format(tyP))
      var time = System.currentTimeMillis()
      println("took %d seconds to read and load adj file".format(elapsed(time)))
      if (DEBUG_GRAPH)
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
      val destination = GraphVertexGen(dst, 100)

      /** process the vertex **/
      println("processing vertex:" + vertex.name + " to " + destination.name)
      time = System.currentTimeMillis()
      dfs.process(tyP)(vertex)
      println("took %d seconds to process bfs".format(elapsed(time)))

      println("finding paths")
      /** check for paths to vertex against all neighbors of vertex **/
      var cntr = 0
      var total = 0
      var output: List[String] = List[String]()
      var collect = List[(Int, List[String])]()

      if (dfs.hasPathTo(destination)) {
        if (total % 10000 == 0) println("so far:%d".format(total))
        total += 1
        //if (DEBUG)
        println(total + ":" + vertex.name + " to " + destination.name + ": ")

        output = output ::: List[String](vertex.name + " to " + destination.name + ": ")
        time = System.currentTimeMillis()
        for (p <- dfs.pathTo(destination, vertex)) {

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
          println("took %d seconds to collect paths".format(elapsed(time)))
          for ((hops, list) <- collect) {
            list.take(25).foreach(println(_))
            println("hops=%d".format(hops))
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
    val filename = scala.io.StdIn.readLine("%s", "(dgraph only)enter filename")
    val delem = scala.io.StdIn.readLine("%s", "enter dilimeter")
    println("entered:file=%s,delim=%s".format(filename, delem))

    /** get the adjacency map only once **/
    var time = System.currentTimeMillis()
    var adj: Graph[GraphVertexGen[String, Int]] = null
    adj = instantiateGraph[String, Int](delem)(filename, GraphConstants.directed)
    println("took %d seconds to read and load adj file".format(elapsed(time)))
    if (DEBUG_GRAPH)
      adj.printGraph
    println("size=" + adj.getGraph.size)

    /** now loop getting different src and dst data to test **/
    var data: String = "notnull"
    var source: String = null
    var destination: String = null
    var weight: String = null
    var searchType: String = null

    while (!(data == null || data.trim() == "")) {

      data = scala.io.StdIn.readLine("%s", "enter source")
      if (!(data == null || data.trim() == ""))
        source = data

      data = scala.io.StdIn.readLine("%s", "enter destination")
      if (!(data == null || data.trim() == ""))
        destination = data

      data = scala.io.StdIn.readLine("%s", "enter search-type(bfs,dfs)")
      if (!(data == null || data.trim() == "")) {
        searchType = data
        println("entered:src=%s,dst=%s,searchType=%s".format(source, destination, searchType))

        searchType match {
          case "dfs" =>
            testShowConnectionsTwoPointsAdjDfs(
              adj, // adjacency map
              source, // beginning point
              destination, // endpoint
              recursive // use recursion or not
            )
          case "bfs" =>
            testShowConnectionsTwoPointsAdjBfs(
              adj, // adjacency map
              source, // beginning point
              destination, // endpoint
              recursive // use recursion or not
            )
        }

      }

    }
  }
  def convertAlgorithmFormat = {
    
  }
  def main(args: Array[String]) {

    // testDirectedDFS(args)
    // test(GraphConstants.non_recursive)

    /** test using user console input **/
    // getStdin(GraphConstants.non_recursive)
    testDirectedCycle

  }
}