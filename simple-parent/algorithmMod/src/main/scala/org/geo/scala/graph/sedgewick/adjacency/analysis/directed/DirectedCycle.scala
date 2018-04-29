package org.geo.scala.graph.sedgewick.adjacency.analysis.directed

import scala.collection.mutable
import java.util.Stack
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.GraphConstants
import util.control.Breaks._
import scala.annotation.tailrec

/**
 * This class is a first attempt to tranlate Sedgewick's cycle
 * detection algorithm from Java to Scala.
 * It is a work in progress.
 * @author george
 *
 * @param <T>
 */
trait DirectedCycle[T] {
  def process(t: GraphConstants.Value): Unit
  def hasPathTo(v: T): Boolean // is there a path from s to v?
  def pathTo(v: T, s: T): Iterable[T]
  def hasCycle: Boolean
  def cycle: Iterable[T]
}

object DirectedCycle {
  def apply[T](graph: Graph[T]): DirectedCycle[T] =
    new DirectedCycleImpl[T](graph)

  /** private implementation **/
  private class DirectedCycleImpl[T](
    private val graph: Graph[T]) extends DirectedCycle[T] {
    println("instantiating DirectedCycleImpl")
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var marked: mutable.Map[T, Boolean] = _
    private var edgeTo: mutable.Map[T, T] = _
    private var cycleV: java.util.Stack[T] = _
    private var onStack: mutable.Map[T, Boolean] = _

    /** count of all vertices connected to input vertex s **/
    var counter = 0

    var dcounter = 0
    /** used for tracing and debugging **/
    var start = 0l
    /** used for tracing and debugging **/
    /**
     *         private implementations
     */
    private def DEBUG = false

    /**
     *         private implementations
     */

    /**
     * Two versions of a depth first search of a graph used
     * to determine the path between a source and destination
     * vertex in a graph. This works with both undirected and
     * directed graphs. This version deviates from the classical
     * versions given in most texts in that it uses a HashMap to
     * store both vertices and adjacency lists.
     *
     * @see https://github.com/sidhartha11/scalastudy
     */

    /** RECURSIVE VERSION **/

    private def dfsRecursive(graph: Graph[T], v: T): Unit = {
      /** mark v as being seen **/
      marked(v) = true
      /** put v on the stack to check for cycles **/
      onStack(v) = true
      /** count each recursive call  **/
      counter += 1
      /** get the adjacent neighbors of v **/
      breakable {
        for (w <- graph.adj(v)) {
          if (hasCycle) {
            break
          } /** w does not have an edge stored yet **/ else if (!hasPathTo(w)) {
            edgeTo(w) = v
            /** check the adjacent neighors of w **/
            dfsRecursive(graph, w)
            /** there is already an edge to w, if it is also on the stack we have a cycle **/
          } else if (onTheStack(w)) {
            cycleV = new Stack[T]()
            /** collect all edges to v to find the cycle **/
            var x = v
            var notdetected = x != w
            var notonlist = false
            breakable {
              while (notdetected) {
                cycleV.push(x)
                val xt = edgeTo get x
                println("xt=" + xt)
                if (xt == None) {
                  notonlist = true
                  cycleV = null
                  break
                }
                x = xt.get
                notdetected = x != w
              }
              /** end of while **/
            }
            /** end of breakable **/
            if (cycleV != null) {
              cycleV.push(w)
              cycleV.push(v)
            }
          }
          onStack(v) = false
        }
        /** end of for loop **/
      }
    }

    private def dfsRecursiveWithTrace(graph: Graph[T], v: T): Unit = {
      /** mark v as being seen **/
      marked(v) = true
      /** put v on the stack to check for cycles **/
      onStack(v) = true
      /** count each recursive call  **/
      counter += 1
      /** get the adjacent neighbors of v **/
      println("v=" + v)
      breakable {
        println("checking on adj of %s %s".format(v, graph.adj(v)))
        for (w <- graph.adj(v)) {
          if (hasCycle) {
            println("breaking out of for loop due to cycle present")
            break
          } /** w does not have an edge stored yet **/ else if (!hasPathTo(w)) {
            edgeTo(w) = v
            /** check the adjacent neighors of w **/
            dfsRecursive(graph, w)
            /** there is already an edge to w, if it is also on the stack we have a cycle **/
          } else if (onTheStack(w)) {
            println("w on stack = " + w)
            cycleV = new Stack[T]()
            /** collect all edges to v to find the cycle **/
            var x = v
            var notdetected = x != w
            var notonlist = false
            breakable {
              while (notdetected) {
                println("v=" + v + ",x=" + x + ",w=" + w)
                println("edgeTo = %s".format(edgeTo))
                cycleV.push(x)
                val xt = edgeTo get x
                println("xt=" + xt)
                if (xt == None) {
                  println("None detected")
                  println(">>x = " + x)
                  println(">>edgeTo = " + edgeTo)
                  notonlist = true
                  cycleV = null
                  break
                }
                x = xt get

                notdetected = x != w
              }
              /** end of while **/
            }
            /** end of breakable **/
            if (cycleV != null) {
              cycleV.push(w)
              cycleV.push(v)
            }
          }
          onStack(v) = false
        }
        /** end of for loop **/
      }
    }

    /** ITERATIVE VERSION **/
    private def dfs(graph: Graph[T], v: T): Unit = {
      val stack = mutable.Stack[T]()
      /** push the first element onto the stack **/
      stack.push(v)
      while (!stack.isEmpty) {
        val s = stack.pop()
        if (!hasPathTo(s)) {
          counter += 1
          marked(s) = true
          /** get all the adjacent vertices of s **/
          /** push each one that is not marked onto the stack **/
          for (w <- graph.adj(s)) {
            if (!hasPathTo(w)) {
              edgeTo += (w -> s)
              stack.push(w)
            }
          }
        }
      }
    }

    override def toString = "DirectedCycle:" + counter

    /**
     *          public api implementation
     */

    /**
     * @return the number of verticies connect to input
     * source vertex
     */
    def count: Int = counter

    def hasPathTo(v: T): Boolean = {
      val t = marked get v
      if (t == None) false
      else t get
    }

    def onTheStack(v: T): Boolean = {
      val t = marked get v
      if (t == None) false
      else t get
    }

    def pathTo(v: T, s: T): Iterable[T] = {

      if (DEBUG) {
        start = System.currentTimeMillis()
        dcounter += 1
        println("%d:%d:finding path from %s to %s".format(edgeTo.size, dcounter, s, v))
      }

      if (!hasPathTo(v)) {
        Iterable.empty[T]
      } else {
        var path = mutable.Buffer[T]()
        var x = v
        /** adding **/
        x +=: path
        while (x != s) {
          x = edgeTo(x)
          x +=: path
        }
        /** commented out **/
        // s +=: path

        if (DEBUG) {
          println("elapsed time:%d".format((System.currentTimeMillis() - start) / 1000))
          println("buffer size returning:%d".format(path.size))
        }

        path
      }
    }

    def hasCycle: Boolean = cycleV != null && !cycleV.isEmpty

    /**
     * Since this method relies on a java.util.Stack and the
     * requirement is to return an Iterable, need to convert
     * the stack to a mutable.ArrayBuffer so that it can be
     * returned to the user as an Iterable.
     */
    def cycle: Iterable[T] = {
      var itr: mutable.ArrayBuffer[T] = mutable.ArrayBuffer[T]()
      while (cycleV.isEmpty == false) {
        itr += cycleV.pop
      }
      itr
    }

    def process(recurType: GraphConstants.Value): Unit = {
      /** map used to determine is a vertex is connected to the input vertex,s **/
      marked = new mutable.HashMap[T, Boolean]()
      edgeTo = new mutable.HashMap[T, T]()
      cycleV = new java.util.Stack[T]()
      onStack = new mutable.HashMap[T, Boolean]()
      for (s <- graph.getGraph.keySet if !hasPathTo(s))
        recurType match {
          case GraphConstants.recursive     => dfsRecursive(graph, s)
          case GraphConstants.non_recursive => dfs(graph, s)
        }

      /** debug code **/
      if (DEBUG) {
        for ((k, v) <- marked) {
          println(k + "," + v)
        }
        println(marked)
      }
    }
  }
}