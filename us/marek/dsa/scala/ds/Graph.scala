package us.marek.dsa.scala.ds

// needed to rename Stack since I also have my own implementation in this package
import scala.collection.mutable.{ HashMap, HashSet, LinkedHashSet, Stack => CStack }


// let's test the code
object Graph extends App {

  implicit def stringToVertex(s: String) = new Vertex(s)
  
  // some test data 
  
  val graph = new Graph[String, Option[Int]]() {
    "A,B,C,D,E,F,G,H,I".split(",").foreach(s => addVertex(s))
    addUndirectedEdge("A", "B", None)  // we don't care about the weights for now, so weights = None
    addUndirectedEdge("B", "F", None)
    addUndirectedEdge("F", "H", None)
    addUndirectedEdge("A", "C", None)
    addUndirectedEdge("A", "D", None)
    addUndirectedEdge("D", "G", None)
    addUndirectedEdge("G", "I", None)
    addUndirectedEdge("A", "E", None)
  }
  
  /* test depth-first and breadth-first search, so far, the implementation
     is a traversal, not a search, but that will change*/
  
  val start = Vertex("A")
  val f = (v: Vertex[String]) => println(s"visiting $v")
  
  println("\nDepth-first search\n")
  graph.depthFirstSearch(start, f)
  
  println("\nBreadth-first search\n")
  graph.breadthFirstSearch(start, f)
} // end object Graph


/* collections for search - unify API
   the logic - DFS and BFS work exactly the same way,
   but DFS uses a stack while BFS uses a queue,
   so we can "inject" the need collection and write
   the search once 
   note that for Dijkstra's algorithm, we will need
   a priority queue/heap, so if we make the method 
   general enough, all we'll need to do is provide
   the correct data structure (in Dijkstra's case, 
   path will matter, too)
 */
sealed trait GraphCollection[T] {
  def add(t: T): Unit
  def remove: Option[T]
  def peek: Option[T]
  def isEmpty: Boolean
}

case class GraphQueue[T] extends GraphCollection[T] {
  private val q = new scala.collection.mutable.Queue[T]()
  override def add(t: T) = q.enqueue(t)
  override def remove = if (!q.isEmpty) Some(q.dequeue) else None
  override def peek = if (!q.isEmpty) Some(q.front) else None
  override def isEmpty = q.isEmpty
  override def toString = s"GraphQueue(${q.toList.mkString(",")})"
}
  
case class GraphStack[T] extends GraphCollection[T] {
  private val s = new scala.collection.mutable.Stack[T]()
  override def add(t: T) = s.push(t)
  override def remove = if (!s.isEmpty) Some(s.pop) else None
  override def peek = if (!s.isEmpty) Some(s.top) else None
  override def isEmpty = s.isEmpty
  override def toString = s"GraphStack(${s.toList.mkString(",")})"
} 
// end collection abstraction


/* let's get equals/hashCode/toString etc.
 get rid of type erasure and restrict to ordered types (e.g. for topo sorting, etc.) */
case class Vertex[T <% Ordered[T]: Manifest](value: T)


/* graph edges, get rid of type erasure, require ordering since vertices are ordered
   may need that for topological sorting, etc. */
case class Edge[T <% Ordered[T]: Manifest, S <% Ordered[S]: Manifest]
  (from: Vertex[T], to: Vertex[T], weight: Option[S] = None) extends Ordered[Edge[T, S]] {

  // needed for the Ordered view bound
  def compare(that: Edge[T, S]): Int = (this.weight, that.weight) match {
    case (Some(x), Some(y)) => if (x == y) 0 else if (x < y) 1 else -1
    case _ => throw new UnsupportedOperationException("Can't compare edges with no weights")
  }

  // ignore weight for equals, since some graphs will be unweighted
  override def equals(that: Any) = that match {
    case Edge(f, t, _) => this.from == f && this.to == t
    case _ => false
  }

} // end class Edge


// finally, the main graph class !!!
class Graph[T <% Ordered[T]: Manifest, S <% Ordered[S]: Manifest] {

  /* I came up with the hash table implementation of the adjacency "matrix" myself,
    but apparently Guido van Rossum is a big promoter of this for adjacency lists:
    http://www.python.org/doc/essays/graphs/
    LinkedHashSet isn't critical (a regular HashSet will do), but it will keep 
    the insertion order to easily compare against test samples
    let's make it a type in case we need to use it in other places */
  type AdjHash = HashMap[Vertex[T], LinkedHashSet[Edge[T, S]]]
  val adjHash = new AdjHash()

  /* the value will be an empty set until we add data, but the vertex will be invisible 
     until it appears in the adjacency hash table*/
  def addVertex(v: Vertex[T]) = adjHash.put(v, LinkedHashSet.empty[Edge[T, S]])
  
  
  // to do - delete vertex !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
  
  // abstraction = :)
  def depthFirstSearch(start: Vertex[T], f: Vertex[T] => Unit) = search(start, f, new GraphStack[Vertex[T]]())
  def breadthFirstSearch(start: Vertex[T], f: Vertex[T] => Unit) = search(start, f, new GraphQueue[Vertex[T]]())
  
  // accept different data structures (stack, queue) and genetate different search behavior (DFS, BFS)
  private def search(start: Vertex[T], f: Vertex[T] => Unit, collection: GraphCollection[Vertex[T]]) = {
    println("In search... :))")
    require(adjHash.contains(start), s"starting vertex $start not in graph")
    
    val visited = new HashSet[Vertex[T]]()
    
    def getUnvisited(h: LinkedHashSet[Edge[T,S]]) = h.find(v => !visited.contains(v.to))
    
    collection.add(start)
    visited.add(start)
    f(start)
    
    while (!collection.isEmpty) {
      println(s"collection = $collection")
      adjHash.get(collection.peek.get) match {
        case Some(x) => {
          val e = getUnvisited(x)
          if (e != None) {
            val v = e.get.to
            f(v)
            visited.add(v)
            collection.add(v)
          } else {
            collection.remove
          }
        }
        case _ =>
      }
    } 
  }

}