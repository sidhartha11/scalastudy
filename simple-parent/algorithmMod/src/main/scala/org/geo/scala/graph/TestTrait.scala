package org.geo.scala.graph

import org.geo.scala.graph.sedgewick.adjacency.EdgeWeightedGraph
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.sedgewick.GraphUtilitiesEdge._

/**
 * Example of creating a trait that is extended by an internal private
 * class within the compainion object of the trait.
 * This perplexing class from a Java programmer's point of view
 * and a scala novice's point of view.
 * Objective:
 * To create a public api as a trait ( analogy in java , Interface )
 * To have a class that is Sortable extend that trait.
 *
 * What I seemingly had to do to accomplish this:
 * 1. have TestTrait extend the Ordered trait passing the
 * generic TestTrait as an type parameter.
 *
 * 2. Since I wanted to use the "apply method" I created a companion
 * object to that TestTrait. I also, for whatever google induced reason
 * I discovered, added an implicit ordering parameter list as an additional
 * argument to the apply method. The apply method is responsible for instantiating
 * an actual TestTraitImpl implementation when invoked.
 *
 * 3. The implementation of TestTraitImpl requires inclusion of the extra
 * parameter list for the ordering passed in from the apply method.
 *
 * 4. The compare method must be implemented in TestTraitImpl since it is
 * an abstract method in the Ordered trait which is extended by TestTrait which
 * in turn is extended by class TestTraitImpl
 */
trait TestTrait[T, W] {
  def weight: W
  def either: T
  def other(v: T): T
  /**
   * the compare method is used to compare the weights of 
   * edges. The Apply function declares an implicit order parameter
   * that will need to be supplied at compile time by every type of 
   * Type parameter used for W.
   * 
   */
def compare[T: Ordering] (o1:T, o2:T):Boolean
 
}

object TestTrait {
  
  /**
   * implicit ordering: MyOrdering[W] 
   * This will have to be supplied implicitly for every different combination 
   * of Type parameters used by this method.
   * 
   */
  def apply[T, W](v: T, w: T, wt: W): TestTrait[T, W] =
    new TestTraitImpl[T, W](
      v, w, wt)

  private case class TestTraitImpl[T, W](
    private val v: T, private val w: T,
    private val wt: W) extends TestTrait[T, W] {

    override def equals(o: Any) = o match {
      case that: TestTraitImpl[T, W] => that.v.equals(this.v) && that.w.equals(this.w)
      case _                         => false
    }
    override def hashCode = v.hashCode + w.hashCode
    override def toString = // "TestTraitImpl: " + v + "," + w + "," + wt
    "%s-%s %.5f".format(v,w,wt)
//    def compare(me: TestTrait[T,W], that: TestTrait[T,W]) = {
//      ordering.compare(me,that)
//    }
    def either: T = v
    

    
    def other(vertex: T): T = {
      if (vertex == v)
        w
      else if (vertex == w)
        v
      else
        throw new IllegalArgumentException("Inconsistent Edge")
    }
    def weight: W = wt
  }

} // end companion object

object RunnerTestTrait {

  def main(args: Array[String]) {
    println("running")
//    val testTrait = TestTrait[String, Double]("12", "12", 13.0)
//
//    val list: List[TestTrait[String, Double]] =
//      List(
//        TestTrait[String, Double]("12", "12", 13.0), TestTrait[String, Double]("13", "13", 14.0), TestTrait[String, Double]("14", "14", 15.0), TestTrait[String, Double]("2", "2", 2.0), TestTrait[String, Double]("1", "1", 1.0))
//
//    val list2: List[TestTrait[GraphVertexGen[String, Int], Double]] =
//      List(
//        TestTrait[GraphVertexGen[String, Int], Double](GraphVertexGen[String, Int]("12", 100), GraphVertexGen[String, Int]("13", 100), 13.0), TestTrait[GraphVertexGen[String, Int], Double](GraphVertexGen[String, Int]("14", 100), GraphVertexGen[String, Int]("15", 100), 1.0), TestTrait[GraphVertexGen[String, Int], Double](GraphVertexGen[String, Int]("16", 100), GraphVertexGen[String, Int]("17", 100), 18), TestTrait[GraphVertexGen[String, Int], Double](GraphVertexGen[String, Int]("18", 100), GraphVertexGen[String, Int]("19", 100), 5))

//    println("testTrait = %s".format(testTrait))
//    println("list = %s".format(list))
//    println("list.sorted = %s".format(list.sorted))
//
//    println("list2 = %s".format(list2))
//    println("list2.sorted = %s".format(list2.sorted))
   
    
    /** create and load EdgeWeightedGraph object **/
//    val wGraph1 = instantiateGraph[T,W <: Ordered[W]](delem: String)(filename: String, dir: GraphConstants.Value = GraphConstants.undirected) = {
    val wGraph1 = instantiateGraph(",")("tinyEWG.txt", GraphConstants.directed)
    /** print Iterable of weighted edges **/
    for ( (key,edge) <- wGraph1.edges ) {
      println("vertex = %s , edge = %s".format(key,edge))
    }
//    val wGraph = EdgeWeightedGraph[GraphVertexGen[String, Int],Double](GraphConstants.directed)
//    wGraph.addEdge(
//     TestTrait[GraphVertexGen[String, Int], Double](GraphVertexGen[String, Int]("12", 100), GraphVertexGen[String, Int]("13", 100), 13.0))
  }
}