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
import scala.collection.mutable.ArrayBuffer
import java.io.InputStream
import scala.io.Codec

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
  def readGraph[T, W](delem: String)(file: String): ArrayBuffer[(T, T, W)] = {
    println("reading file:" + file)
    println("delem=" + delem)
    var buffer = ArrayBuffer[(T, T, W)]()
    val source = Source.fromFile(file)
    for (line <- source.getLines()) {
      val t = line split ("\\" + delem)
      if (t.size != 3) {
        println("skipping , 3 elements required, v delim v delim wt" + line)
      } else {
        /**
         * simple syntax check making sure the the third
         * field is numeric
         */
        if (t(2) isNumeric ()) {
          buffer += ((t(0).trim.asInstanceOf[T], t(1).trim.asInstanceOf[T], t(2).trim.asInstanceOf[W]))
        } else {
          println("skipped, 3rd field not numeric")
        }
      }
    }
    buffer
  }
  /** utilities that load the vertices into a list only, for testing equals and compare functionality **/
  def loadGraph[T, W](base: String, filename: String): mutable.ArrayBuffer[GraphVertexGen[T, W]] = {
    loadGraphInnerCompare[T, W](base.trim() + filename)
  }

  def loadGraphInnerCompare[T, W](filename: String): mutable.ArrayBuffer[GraphVertexGen[T, W]] = {

    /**
     * Populate the graph with data from input file
     */
    println("Loading Vertex Data")
    var counter = 0
    var skipped = 0
    var list = mutable.ArrayBuffer[GraphVertexGen[T, W]]()
    for ((node, neighbor, weight) <- readGraph[T, W](",")(filename)) {
      counter += 2
      list += GraphVertexGen(node, weight)
      list += GraphVertexGen(neighbor, weight)
      //        println("adding:(%s,%s,%d)".format(node, neighbor, weight))
    }
    println("read #" + counter + " records")
    list
  }
  def initializeGraph[T, W](based: String, filename: String, biDir: GraphConstants.Value): Graph[GraphVertexGen[T, W]] = {
    initializeGraphInner(based.trim() + filename, biDir)
  }
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
      println("i=%s, i.size=%d, %s".format(i, i.size, i.getClass))
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
   /**
   * Create a graph object, populate with test file input
   * directional/undirectional based on biDir
   * If the file name is a full path name, do not prepend the base directory.
   * TBD: make the non full path get the file from the class path.
   */
  def initializeGraph[T, W](filename: String, biDir: GraphConstants.Value): Graph[GraphVertexGen[T, W]] = {
    if (fullPath(filename)) {
      initializeGraphInner(filename, biDir)
    } else {
      initializeGraphInner(base.trim() + filename, biDir)
    }
  }
  def testLoadingDataIntoBuffer {
    val l = loadGraph[String, Int](base, CITIES)
    println("\nunsorted\n")
    for (x <- l) println(x)
    println("\nsorted\n")
    l.sortBy(_.name).foreach(println)
  }
  def initializeGraphInner[T, W](filename: String, biDir: GraphConstants.Value): Graph[GraphVertexGen[T, W]] = {
    /**
     * Create a Graph Object
     */
    val a: Graph[GraphVertexGen[T, W]] = Graph(biDir)
    /**
     * Populate the graph with data from input file
     */
    println("adding nodes")
    var counter = 0
    var skipped = 0
    for ((node, neighbor, weight) <- readGraph[T, W](",")(filename)) {
      counter += 1
      //        println("adding:(%s,%s,%d)".format(node, neighbor, weight))
      a.addEdge(GraphVertexGen[T, W](node, weight), GraphVertexGen[T, W](neighbor, weight))
    }
    println("read #" + counter + " records")
    a
  }
  def main(args: Array[String]) {
  def fullPath(filename: String) = {
    val namePattern = "^[A-Z]:|^\\/".r
    println("analyzing filename:" + filename)
    /** see if first occurrence if a full path name **/
    val b = namePattern.findFirstIn(filename)
    b != None
  }
  def fromResource(resource: String, classLoader: ClassLoader = Thread.currentThread().getContextClassLoader())(implicit codec: Codec): InputStream = {
    println("calling classLoader")
    val l = classLoader.getResourceAsStream(resource)
    println("got " + l)
    //  var i = l.read()
    //  while ( i != -1 ) {
    //       print(i.asInstanceOf[Char])
    //       i = l.read()
    //  }
    //    println("reading one char")
    //    val i:Char = ldr.read().asInstanceOf[Char]
    //    println("i=" + i)
    println("returning l:" + l)
    l
  }

  def niceFeedbackReadResource(resource: String): InputStream = {

    val l = fromResource(resource)
    println("got InputStream:" + l)
    l
  }

  def testfullPath = {
    val filename = "C:\\temp\\test\\some.txt"
    val filename2 = "/some/linux/file.txt"
    val filename3 = "file.txt"

    println(fullPath(filename))
    println(fullPath(filename2))
    println(fullPath(filename3))
  }

  def testRawRead = {
    /** test read from class path **/
    val l = niceFeedbackReadResource("rawCities.txt")
    var i = l.read()
    while (i != -1) {
      print(i.asInstanceOf[Char])
      i = l.read()
    }
  }
  //  /** test read from class path **/
  //  val l = niceFeedbackReadResource("rawCities.txt")
  //  var i = l.read()
  //  while ( i != -1 ) {
  //       print(i.asInstanceOf[Char])
  //       i = l.read()
  //  }

  val readmeText: Iterator[String] = Source.fromResource("rawCities.txt").getLines
  for (i <- readmeText) println(i)
    val b = syntheticDataRaw("C:\\temp\\rawCities.txt")
    for (l <- b) println("left=%s, right=%s,weight=%d".format(l._1, l._2, l._3))
    //   testTraceHopListRandom("Washington, Kansas", RAWCITIES, GraphConstants.non_recursive)
    //   testTraceHopListBRandom("Washington, Kansas", RAWCITIES, GraphConstants.non_recursive)

  }
}