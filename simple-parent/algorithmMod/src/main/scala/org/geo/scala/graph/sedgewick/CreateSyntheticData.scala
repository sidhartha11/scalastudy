package org.geo.scala.graph.sedgewick
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.analysis.BreadthFirstPaths
object CreateSyntheticData {

  def DEBUG = true
  def DEBUG_GRAPH = false
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

    //   testTraceHopListRandom("Washington, Kansas", RAWCITIES, GraphConstants.non_recursive)
    //   testTraceHopListBRandom("Washington, Kansas", RAWCITIES, GraphConstants.non_recursive)

  }
}