package org.geo.scala.sorts.testData

import org.geo.scala.sorts.quicksorting.QuickSort._
import org.geo.scala.sorts.insertionSorting.InsertionSort._


case class Person(name:String, age:Int) extends Ordered[Person] {
  def compare(that: Person) = {
    if (this.name.toLowerCase() < that.name.toLowerCase() ) {
      println("-1")
      -1
    } else if ( this.name.toLowerCase() > that.name.toLowerCase() ){
        println("1")
      1
    
    } else {
      println("0")
      0
    }
      
  }
}
object TestPerson extends App {
val people: Array[Person] = Array(
    Person("Xia", 50),
    Person("bob", 30),
    Person("ann", 32),
    Person("carl", 19)
    )
//qSort(people)
iSort(people)
people.foreach { println }
}