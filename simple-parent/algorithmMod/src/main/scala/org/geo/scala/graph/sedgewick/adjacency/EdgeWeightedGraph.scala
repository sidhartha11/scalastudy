package org.geo.scala.graph.sedgewick.adjacency

import scala.collection.mutable

import org.geo.scala.graph.GraphConstants

/**
 * @author George Curington
 * @version 1.0
 * @since 4/14/2018
 * <pre>
 * trait EdgeWeightedGraph[T]
 *
 * This is a trait. It is used to give a public interface
 * to users the Edge Weighted Adjacency Structure.
 * This version of the adjacency structure is based solely on
 * hash maps.
 * The list of vertices is a Map containing a key for
 * the vertex and a value for the list of neighbor vertices.
 * The list of neighbor vertices is also a Map. There is some
 * additional space used to represent the vertices since it is
 * not necessary to contain a complete vertex object as the key.
 * But this implementation does just that. The vertex object, represented
 * buy the type "T" generic parameter can be any class object that
 * overrided hashcode and equals.
 *
 * </pre>
 */
trait EdgeWeightedGraph[T] {
  def addEdge(v: T, w: T) // add edge v-w to this graph
  def adj(v: T): Iterable[T] // vertices adjacent to v
  def adjreverse(v: T): Iterable[T]
  def V: Int // number of vertices
  def E: Int // number edges
  def printGraph: Unit // print the contents of graph
  def getGraph: mutable.LinkedHashMap[T, mutable.LinkedHashMap[T, T]]
  def reverseGraph: EdgeWeightedGraph[T]
}

/**
 * This is a companion object. It defines an apply method that is
 * used to allow for the construction of an adjacency structure using
 * this form:
 * Graph(GraphConstants.<somedirection>)
 * The actual implementation of the adjacency structure is private inside
 * the companion object:
 * private class EdgeWeightedGraphImpl[T]
 * 
 * The apply method of the companion object will create a digraph by default as 
 * per the default parameter:
 * apply[T](biDir: GraphConstants.Value = GraphConstants.directed)
 *
 */
object EdgeWeightedGraph {
  def apply[T](biDir: GraphConstants.Value = GraphConstants.directed): EdgeWeightedGraph[T] =
    new EdgeWeightedGraphImpl[T](biDir)

  /**
   * <pre>
   * This version of the adjacency structure uses maps and the underlying
   * data structure to store both vertices and adjacency lists. Here the
   * lists are also maps.
   * The main map:
   * graphMap: mutable.LinkedHashMap[T, mutable.LinkedHashMap[T, T]]
   * Stores a vertex dynamically when it is first encountered. In this
   * implementation, vertices are not stored individually up front.
   * They are stored as pairs of neighbor vertices. This implementation
   * can be directed or undirected based on the biDir parameter.
   * Input is of the following form:
   * vertex, vertex, weight
   *
   * example:
   * Philadelphia, Pittsburgh,100
   * Boston, New York,100
   * Hartford, New York,100
   * Los Angeles, San Diego,100
   *
   * When the adjacency structure is first created, an empy graph
   * is instatiated:
   * new mutable.LinkedHashMap[T, mutable.LinkedHashMap[T, T]]()
   *
   * Note that the LinkedHashMap could just as well be HashMap.
   *
   * vertex pairs are added by repeatedly calling:
   * addEdge(vertex,vertex)
   * Depending on whether the map is directed or undirected this
   * call will make 1 or 2 map insertions respectively.
   *
   * @param <T> the vertex object
   */
  private class EdgeWeightedGraphImpl[T](
    private val biDir: GraphConstants.Value) extends EdgeWeightedGraph[T] {

    /** number of vertices in the graph **/
    private[this] var numberVertices = 0
    private[this] var numberEdges = 0
    /** create an empty map of maps to store the vertices and edges **/
    private val graphMap: mutable.LinkedHashMap[T, mutable.LinkedHashMap[T, T]] = new mutable.LinkedHashMap[T, mutable.LinkedHashMap[T, T]]()

    /** DISPLAY DIRECTION **/
    println("mapped adj " + biDir)

    def addEdge(u: T, v: T): Unit = {
      /** if biDir then must update both as neighbors **/
      biDir match {
        case GraphConstants.undirected => undirectedInsertion(u, v)
        case GraphConstants.directed   => directedInsertion(u, v)
      }
    }
    /**
     * <pre>
     * Note the following inefficiency: The adjacency lists are represented as
     * maps. The key and value of the map entry is simply a duplicate of the
     * vertex being inserted. The vertex contains various information not known
     * by the adjacency structure, i.e. weight, color or any type of attribute.
     * This data is used by the client of the adjacency structure to perform
     * various types of graph processing. My current understanding of scala
     * prevents me from adding a level of efficient processing that utilizes
     * the components of the vertex to store the mapping in a space-wise
     * efficient manner. For the sake of simplicity, I just store the vertex
     * as a key and value. I might change this later and put a default boolean
     * as the value.
     * </pre>
     */
    private def addMapping(u: T, v: T): Unit = {
      /** get the value for key u **/
      var t = graphMap get u
      /** if there is no mapping, the Optional None is returned **/
      if (t == None) {
        /** this is a new vertex, so update both edge count and vertex count **/
        numberVertices += 1
        numberEdges += 1

        /** create a new hash map entry for the edge being inserted for this vertex **/
        val r = new mutable.LinkedHashMap[T, T]()
        /** add a key, value pair to this new hashmap of edges **/
        r += (v -> v)
        /** this new Vertex to the Adjacency Structure **/
        graphMap += (u -> r)
      } else {
        /** update the list of edges for this known vertex **/
        numberEdges += 1
        t.get += (v -> v)
      }
    }
    private def addMappingDirected(u: T, v: T): Unit = {
      var t = graphMap get u
      if (t == None) {
        /** update number of vertices and edges since this vertex does not exist **/
        numberVertices += 1
        numberEdges += 1
        /** create an entry for the adj list **/
        val r = new mutable.LinkedHashMap[T, T]()
        /** add the neighbor, v to the adj list **/
        r += (v -> v)
        /** update the vertex object **/
        graphMap += (u -> r)
      } else {
        /** vertex is already present, so only count the edges **/
        numberEdges += 1
        /** add the new neighor to the adj map, note duplicates can not happen here **/
        t.get += (v -> v)
      }

      /** now check to see if the neighbor, v, exists in the vertex map only, if not **/
      /** put an entry there with an empty adj list for reference **/
      t = graphMap get v
      if (t == None) {
        /** only update the number of vertexies , since this is a on directional mapping only **/
        numberVertices += 1
        val r = mutable.LinkedHashMap[T, T]()
        /** put an empty place holder for the one directional vertex **/
        graphMap += (v -> r)
      }
    }
    private def directedInsertion(u: T, v: T): Unit = {
      /** directed map **/
      addMappingDirected(u, v)
    }

    private def undirectedInsertion(u: T, v: T): Unit = {
      /** get the first mapping **/
      addMapping(u, v)
      /** get the second side **/
      addMapping(v, u)
    }

    /*
     * <pre>
     * This function simply scans the graphMap and prints out all the
     * vertices along with their associated list of neighbors
     * The first for comprehension retrieves each vertex,neighbor-list pair.
     * The second for comprehension retrieves each vertex from the neighbor-list.
     * </pre>
     *
     * @see org.geo.scala.graph.sedgewick.adjacency.EdgeWeightedGraph#printGraph()
     */
    def printGraph = {
      numberEdges = 0
      numberVertices = 0
      for ((key, list) <- graphMap) {
        numberVertices += 1
        print("\nVertex|" + key + "| ")
        for ((e1, e2) <- list) {
          numberEdges += 1
          print(e2 + "->")
        }
        print("nil")
      }
      println("\n...")
    }

    def adj(v: T): Iterable[T] = {
      val a = graphMap get v
      if (a == None) {
        Iterable.empty[T]
      } else {
        a.get.values
      }
    }

    def adjreverse(v: T): Iterable[T] = {
      val a = graphMap get v
      if (a == None) {
        Iterable.empty[T]
      } else {
        a.get.values.toSeq.reverse.toIterable
      }
    }

    /**
     * returns the number of vertices
     * @see org.geo.scala.graph.undirected.EdgeWeightedGraph#V()
     */
    def V: Int = numberVertices

    /**
     * returns the number of edges
     * @see org.geo.scala.graph.undirected.EdgeWeightedGraph#E()
     */
    def E: Int = {
      biDir match {
        case GraphConstants.undirected => numberEdges / 2
        case GraphConstants.directed   => numberEdges
      }
    }

    override def toString = "EdgeWeightedGraph:" + graphMap.mkString("[", ",", "]")

    def getGraph: mutable.LinkedHashMap[T, mutable.LinkedHashMap[T, T]] = graphMap
    
    /**
     * The simple matter of reversing a digraph is to just 
     * added the vertices of the current graph in reverse order by
     * forcing each vertex to point to the original originating vertex.
     * The restriction here is that this must only be applied to 
     * a directed graph and not a undirected graph.
     * @throws This method will throw an illegalaccess exception if you 
     * attempt to reverse a graph that is not a digraph. 
     */
    def reverseGraph: EdgeWeightedGraph[T] = {
      biDir match {
        case GraphConstants.directed => 
        case _ => throw new IllegalAccessException("Only can reverse digraph")
      }
      val revGraph:EdgeWeightedGraph[T] = EdgeWeightedGraph[T]()
      for ( (k,v) <- getGraph ) {
        for ( (k1,v1 ) <- v ) {
          revGraph.addEdge(v1,k) 
        }
      }
      revGraph
    }
  }
}