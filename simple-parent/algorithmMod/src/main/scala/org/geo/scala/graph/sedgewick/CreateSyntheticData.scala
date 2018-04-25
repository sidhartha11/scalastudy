package org.geo.scala.graph.sedgewick

import scala.io.Source
import scala.collection.mutable
import sys.process._
import java.net.URL
import java.io.File
import java.util.concurrent.ThreadLocalRandom

import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.analysis.BreadthFirstPaths
object CreateSyntheticData {
  var cache: mutable.Map[String, Int] = mutable.Map[String, Int]()

  def PRINTRANDOM = false
  def clearCache: Unit = {
    println("clearing cache")
    cache.clear
  }
  def base =
    """|C:/githubstuff/javaprojs/studystuff
      |/scalastudy/simple-parent/algorithmMod
      |/src/main
      |/resources/
      """.stripMargin.replaceAll("\n", "")
  def filename = "cities.txt"

  def loadGraphRandom[T, W](base: String, filename: String, a: Graph[GraphVertexGen[T, W]]): Graph[GraphVertexGen[T, W]] = {
    loadGraphInnerRandom[T, W](base.trim() + filename, a)
  }
  /**
   * This method is used to read a list of graph data
   * Vertex, Vertex, edgeweight
   * This method seems to be inlined by the JVM
   * because of strange things happening inside the
   * for loop. It seems to be optimized by the JVM
   */
  def readGraph(file: String): List[(String, String, Int)] = {
    println("reading file:" + file)
    val source = Source.fromFile(file)
    val triples = for (line <- source.getLines()) yield {
      val t = line split (",")
      (t(0).trim, t(1).trim, t(2).trim.toInt)
    }
    source.close()
    triples.toList
  }

  def downloadFile(urlname: String, filename: String): Unit = {

    new URL(urlname) #> new File(filename) !!
  }
  def instantiateGraphRandom[T, W](filename: String) = {
    println("creating adjacency structure")
    var adj = createAdjacencyMap[T, W](GraphConstants.undirected)
    if (DEBUG)
      outPut("adj=%s".format(adj))
    /**
     * load the structure with data
     */
    adj = loadGraphRandom[T, W](base, filename, adj)
    adj
  }
  def readFile(file: String): mutable.ArrayBuffer[String] = {
    println("reading file:" + file)
    val source = Source.fromFile(file)
    var buf = mutable.ArrayBuffer[String]()
    for (line <- source.getLines()) {
      // println(line)
      buf += line.trim
    }
    source.close()
    buf
  }
  def syntheticDataRaw[T, W](filename: String): List[(T, T, W)] = {
    val input = readFile(filename)
    val buf = mutable.ArrayBuffer[(T, T, W)]()
    for (i <- 0 until 400000) {
      val left = ThreadLocalRandom.current().nextInt(0, input.size)
      val right = ThreadLocalRandom.current().nextInt(0, input.size)
      val weight = ThreadLocalRandom.current().nextInt(100, 201)
      buf += ((input(left).asInstanceOf[T], input(right).asInstanceOf[T], weight.asInstanceOf[W]))
    }
    if (PRINTRANDOM) {
      println("dumping random values")
      for (b <- buf) println(b._1 + "^" + b._2 + "^" + b._3)
    }
    buf.toList
  }

  def syntheticData[T, W](filename: String): List[(T, T, W)] = {
    val input = readFile(filename)
    val buf = mutable.ArrayBuffer[(T, T, W)]()
    for (i <- input) {
      println("i=%s, i.size=%d, %s".format(i,i.size, i.getClass))
      val elements = i split ("\\^")
      //        println(elements)
      val left = elements(0)
      val right = elements(1)
      val weitht = elements(2)
      val weight = ThreadLocalRandom.current().nextInt(100, 201)
      buf += ((left.asInstanceOf[T], right.asInstanceOf[T], weight.asInstanceOf[W]))
    }
    if (PRINTRANDOM) {
      println("dumping random values")
      for (b <- buf) println(b._1 + "^" + b._2 + "^" + b._3)
    }
    buf.toList
  }
  def DEBUG = true
  def DEBUG_GRAPH = false
  def loadGraphInnerRandom[T, W](filename: String, a: Graph[GraphVertexGen[T, W]]): Graph[GraphVertexGen[T, W]] = {

    /**
     * Populate the graph with data from input file
     */
    println("adding nodes")
    var counter = 0
    var skipped = 0
    for ((node, neighbor, weight) <- syntheticData[T, W](filename)) {
      counter += 1
      //        println("adding:(%s,%s,%d)".format(node, neighbor, weight))
      a.addEdge(GraphVertexGen[T, W](node, weight), GraphVertexGen[T, W](neighbor, weight))
    }
    println("read #" + counter + " records")
    a
  }
  def testTraceHopListRandom(src: String, file: String, tyP: GraphConstants.Value) =
    {
      println("testing:%s".format(tyP))
      /** create an adjacency object **/
      var time = System.currentTimeMillis()
      val adj = instantiateGraphRandom[String, Int](file)
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
    }

  def testTraceHopListBRandom(src: String, file: String, tyP: GraphConstants.Value) =
    {
      println("testing:%s".format(tyP))
      /** create an adjacency object **/
      var time = System.currentTimeMillis()
      val adj = instantiateGraphRandom[String, Int](file)
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

        })
      if (DEBUG)
        println
    }
  def main(args: Array[String]) {
    val b = syntheticDataRaw("C:\\temp\\rawCities.txt")
    for (l <- b) println("left=%s, right=%s,weight=%d".format(l._1, l._2, l._3))
    //   testTraceHopListRandom("Washington, Kansas", RAWCITIES, GraphConstants.non_recursive)
    //   testTraceHopListBRandom("Washington, Kansas", RAWCITIES, GraphConstants.non_recursive)

  }
}