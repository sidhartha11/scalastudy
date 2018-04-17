package org.review.book.programminginscala3rd.pis_33

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

class Json1 extends JavaTokenParsers {
  
  
  def obj: Parser[Map[String, Any]] = 
    "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _)
    
  def arr: Parser[List[Any]] = 
    "["~> repsep(value, ",") <~"]"
    
  def member: Parser[(String, Any)] = 
    stringLiteral~":"~value ^^
      { case name~":"~value => (name, value) }
    
  def value: Parser[Any] = (
      obj
    | arr
    | stringLiteral
    | floatingPointNumber ^^ (_.toDouble)
    | "null"  ^^ (x => null)
    | "true"  ^^ (x => true)
    | "false" ^^ (x => false)
    )
}

object ReadJson2
{
  def filename: String = "address.json"
  def main(args: Array[String]) {  
  val a:Json1 = new Json1()
  var text = "placeholde"
  try {
  println("looking for " + filename)
  val obj = Source.fromResource(filename)
  val readmeText : Iterator[String] = obj.getLines
  text = readmeText.mkString
  println(a.parseAll(a.value,text))
  } 
  catch  {
    case p: Throwable  => println(p)
  }
  
  
  
  }
}