package org.review.book.programminginscala3rd.pis_33

import scala.util.parsing.combinator.JavaTokenParsers
import java.io.FileReader
import scala.io.Source

class Json extends JavaTokenParsers{
  def filename: String = "address.json"
  def value : Parser[Any] = obj | arr | 
  stringLiteral | 
  floatingPointNumber |
  "null" | "true" | "false"
  
  def obj  : Parser[Any] = "{"~repsep(member, ",")~"}"
  
  def arr : Parser[Any] = "["~repsep(value, ",")~"]"
  
  def member: Parser[Any] = stringLiteral~":"~value
}
object ReadJson
{
  def filename: String = "address.json"

  def main(args: Array[String]) {
//  val reader = new FileReader(filename)
  
  val a:Json = new Json()
  var text = "placeholde"
  try {
  println("looking for " + filename)
  val obj = Source.fromResource(filename)
  
  val readmeText : Iterator[String] = obj.getLines
  text = readmeText.mkString
//  text = Source.fromInputStream(getClass.getResourceAsStream(filename)).mkString
  println(a.parseAll(a.value,text))
  } 
  catch  {
    case p: Throwable  => println(p)
  }
  
  
  
  }
}