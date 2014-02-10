package us.marek.dsa.scala.ds

// needed to rename Stack since I also have my own implementation in this package
import scala.collection.mutable.{ HashMap, HashSet, LinkedHashSet, Stack => CStack }
import scala.collection.mutable.ArrayBuffer


// let's test the code
object Graph extends App {

  implicit def stringToVertex(s: String) = new Vertex(s)
  
  // some test data 
  val graph = new Graph[String, Option[Int]]() {
    "A,B,C,D,E,F,G".split(",").foreach(s => addVertex(s))
    addDirectedEdge("A", "B", None)  // we don't care about the weights for now, so weights = None
    addDirectedEdge("B", "D", None)
    addDirectedEdge("B", "E", None)
    addDirectedEdge("A", "C", None)
    addDirectedEdge("C", "F", None)
    addDirectedEdge("F", "G", None)
  }
  
  /* test depth-first and breadth-first search, so far, the implementation
     is a traversal, not a search, but that will change*/
  
  val start = Vertex("A")
  val f = (v: Vertex[String]) => println(s"visiting $v")
  
  println("\nDepth-first search\n")
  graph.depthFirstSearch(start, f)
  
  println("\nBreadth-first search\n")
  graph.breadthFirstSearch(start, f)
  
  println("\nTopological sort\n")
  println(graph.topoSort)
  
  println("\nMinimum unweighted spanning tree\n")
  println(graph.minUnweightedSpanningTree(start))
  
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
case class Vertex[T <% Ordered[T]: Manifest](value: T) {
  override def toString = s"$value"
}


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
  
  override def hashCode = 31 * ((31 * from.hashCode) + to.hashCode)
  
  override def toString = s"($from -> $to, $weight)"

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
  
  private def copyHash(): AdjHash = {
    val newHash = new AdjHash()
    for (key <- adjHash.keySet) {
      newHash(key) = adjHash(key).clone
    }
    newHash
  }
  
  def minUnweightedSpanningTree(start: Vertex[T]) = {
    val edgeBuffer = new ArrayBuffer[Edge[T, S]]()
    val f = (v: Edge[T, S]) => edgeBuffer += v
    dfsSpan(start, f)
    edgeBuffer.toList
  }
  
  def topoSort: List[Vertex[T]] = {
    
    val remaining = this.copyHash
    val sorted = LinkedHashSet[Vertex[T]]()
   
    def nodesWithNoSuccessors = remaining.find(_._2.isEmpty)
      
    while (!(remaining.size == 0)) {
     val x = nodesWithNoSuccessors
     if (x == None) throw new IllegalStateException("Graph has cycles - nodes remain yet there's no node without successors")
     val next = x.get._1
     sorted += next
     remaining -= next
     remaining.foreach(tuple => {
       val (key, value) = tuple
       val vtx = Edge[T, S](from = key, to = next)
       if (value.contains(vtx)) {
         value -= vtx
       } 
     })
    }
    sorted.toList
  }
 
  
  def deleteVertex(vert: Vertex[T]) = {
    require(adjHash.contains(vert), "Can't delete a nonexistent node")
    val outgoing = adjHash.get(vert).get
    // for (i <- outgoing) yield i
    for (i <- outgoing) {
      val incoming = adjHash.get(i.to)
      if (incoming != None) {
        incoming.get.remove(Edge(i.to, vert))
      }
    }
    adjHash.remove(vert)  
  }
  
  def deleteEdge(from: Vertex[T], to: Vertex[T]): Boolean = {
    require(adjHash.contains(from), "can't delete edge if the start node doesn't exist")
    require(adjHash.get(from).getOrElse(None) == None, "can't delete edge since the start node doesn't have any connections")
    val edge = new Edge[T, S](from, to, None)
    require(adjHash.get(from).get.contains(edge), s"Can't delete an edge between $from and $to since it doesn't exit")
    adjHash.get(from).get.remove(edge)
  }

  def addUndirectedEdge(a: Vertex[T], b: Vertex[T], equiWeight: Option[S]) = {
    addDirectedEdge(a, b, equiWeight)
    addDirectedEdge(b, a, equiWeight)
  }

  def addDirectedEdge(from: Vertex[T], to: Vertex[T], weight: Option[S]) = {
    require(adjHash.contains(from), s"Adjacency hash does not contain the starting vertex $from")
    adjHash.get(from).get.add(Edge(from, to, weight))
  }
  
  // abstraction = :)
  def depthFirstSearch(start: Vertex[T], f1: Vertex[T] => Unit): Unit = search(start, f1, new GraphStack[Vertex[T]]())
  private def dfsSpan[R](start: Vertex[T], f2: Edge[T, S] => R): Unit = search(start = start, collection = new GraphStack[Vertex[T]](), f2 = f2)
  def breadthFirstSearch(start: Vertex[T], f1: Vertex[T] => Unit) = search(start, f1, new GraphQueue[Vertex[T]]())
  
  // accept different data structures (stack, queue) and genetate different search behavior (DFS, BFS)
  private def search[R](start: Vertex[T], f1: Vertex[T] => Unit = (v: Vertex[T]) => Unit,
      collection: GraphCollection[Vertex[T]], f2: Edge[T, S] => R = (e: Edge[T, S]) => ()) = {
    require(adjHash.contains(start), s"starting vertex $start not in graph")
    
    val visited = new HashSet[Vertex[T]]()
    
    def getUnvisited(h: LinkedHashSet[Edge[T,S]]) = h.find(v => !visited.contains(v.to))
    
    collection.add(start)
    visited.add(start)
    f1(start)
    
    while (!collection.isEmpty) {
     // println(s"collection = $collection")
      adjHash.get(collection.peek.get) match {
        case Some(x) => {
          val e = getUnvisited(x)
          if (e != None) {
            val v = e.get.to
            f1(v)
            f2(e.get)
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