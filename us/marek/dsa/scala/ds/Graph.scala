package us.marek.dsa.scala.ds
//package graph

import scala.collection.mutable.{ HashMap, HashSet, Stack => CStack }
import scala.reflect.ClassTag

object Graph extends App {

  implicit def stringToVertex(s: String) = new Vertex(s)
  val graph = new Graph[String, Option[Int]]()
  
  "A,B,C,D,E,F,G,H,I".split(",").foreach(s => graph.addVertex(s))
  graph.addUndirectedEdge("A", "B", None)
  graph.addUndirectedEdge("B", "F", None)
  graph.addUndirectedEdge("F", "H", None)
  graph.addUndirectedEdge("A", "C", None)
  graph.addUndirectedEdge("A", "D", None)
  graph.addUndirectedEdge("D", "G", None)
  graph.addUndirectedEdge("G", "I", None)
  graph.addUndirectedEdge("A", "B", None)
  graph.addUndirectedEdge("A", "E", None)
  
  graph.dfs(Vertex("A"), f = s => println(s"visiting $s"))
 
  /*
 "Ajo,Bordo,Colina,Danza,Erizo,Flor".split(",").foreach(s => graph.addVertex(s))
  graph.addUndirectedEdge("Ajo", "Bordo", 6)
  graph.addUndirectedEdge("Ajo", "Danza", 4)
  graph.addUndirectedEdge("Bordo", "Danza", 7)
  graph.addUndirectedEdge("Bordo", "Colina", 10)
  graph.addUndirectedEdge("Bordo", "Erizo", 7)
  graph.addUndirectedEdge("Danza", "Erizo", 12)
  graph.addUndirectedEdge("Danza", "Colina", 8)
  graph.addUndirectedEdge("Colina", "Erizo", 5)
  graph.addUndirectedEdge("Colina", "Flor", 6)
  graph.addUndirectedEdge("Erizo", "Flor", 7)
  
  graph.dfs(Vertex("Ajo"), f = s => println(s"visiting $s"))
  */

}



case class Vertex[T <% Ordered[T]: Manifest](value: T)

case class Edge[T <% Ordered[T]: Manifest, S <% Ordered[S]: Manifest](from: Vertex[T], to: Vertex[T], weight: Option[S] = None) extends Ordered[Edge[T, S]] {

  def compare(that: Edge[T, S]): Int = (this.weight, that.weight) match {
    case (Some(x), Some(y)) => if (x == y) 0 else if (x < y) 1 else -1
    case _ => throw new UnsupportedOperationException("Can't compare edges with no weights")
  }

  override def equals(that: Any) = that match {
    case Edge(f, t, _) => this.from == f && this.to == t
    case _ => false
  }

}

class Graph[T <% Ordered[T]: Manifest, S <% Ordered[S]: Manifest] {

  private var numVert = 0

  type AdjHash = HashMap[Vertex[T], HashSet[Edge[T, S]]]
  val adjHash = new AdjHash()

  def addVertex(v: Vertex[T]) = {
    adjHash.put(v, HashSet.empty[Edge[T, S]])
    numVert += 1
  }

  /*
  def deleteVertex[S](v: Vertex[T]) = {
    require(adjHash.contains(v), "Can't delete a non-existent vertex from adjacency hash")
    for (e <- adjHash.get(v).get) {
      val a = adjHash(e.to)
      val x = Edge[T, S](e.to, e.from)
      if (a.contains(x)) {
        a.remove(x)
      }
        
    }
    numVert -= 1
  }
  * 
  */

  def addUndirectedEdge(a: Vertex[T], b: Vertex[T], equiWeight: S) = {
    addDirectedEdge(a, b, equiWeight)
    addDirectedEdge(b, a, equiWeight)
  }

  def addDirectedEdge(from: Vertex[T], to: Vertex[T], weight: S) = {
    require(adjHash.contains(from), s"Adjacency hash does not contain the starting vertex $from")
    val e = Edge(from, to, Some(weight))
    adjHash.get(from).get.add(e)
  }

  def dfs(start: Vertex[T], f: Vertex[T] => Unit): Unit = {
    
    println("In dfs")
    require(adjHash.contains(start), s"starting vertex $start not in graph")
    val s = new CStack[Vertex[T]]()
    val visited = new HashSet[Vertex[T]]()
    
    def getUnvisited(h: HashSet[Edge[T,S]]) = h.find(v => !visited.contains(v.to))
      /*
       
       adjHash.get(s.top) match {
      case Some(x) => x.dropWhile(y => visited.contains(y.to)).take(1).toSeq(0)
      case None => None
    } 
    
    */
    
    s.push(start)
    visited.add(start)
    f(start)
    
    while (!s.isEmpty) {
      println(s"stack = $s")
      adjHash.get(s.top) match {
        case Some(x) => {
          val e = getUnvisited(x)
          if (e != None) {
            val v = e.get.to
            f(v)
            visited.add(v)
            s.push(v)
          } else {
            s.pop
          }
        }
        case _ =>
      }
      /*
      adjHash.get(s.top) match {
        case x: Some[HashSet[Edge[T, S]]] => for (e <- x.get) {
          if (!visited.contains(e.to)) {
            f(e.to)
            s.push(e.to)
            visited.add(e.to)
          
          }
        }
        s.pop
        case _ =>
      }

      */
    }

  }

}