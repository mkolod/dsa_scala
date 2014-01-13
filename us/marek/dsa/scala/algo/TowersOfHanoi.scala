package us.marek.dsa.scala.algo

object TowersOfHanoi extends App {

  def hanoi[T](data: List[T]) = {

    case class Move(from: Int, inter: Int, to: Int, elem: Option[T] = None) {
      
      def valid(x: Int) = x >= 1 && x <= 3
      require(List(from, inter, to).forall(valid))
      
      override def toString = s"${elem.get} from $from to $to"
    }

    def recHanoi(elems: List[T], move: Move): List[Move] = {

      def default(t: T) = List(move.copy(elem = Some(t)))

      elems match {
        
        case Nil => Nil

        case x :: ys => {
          recHanoi(ys, Move(move.from, move.to, move.inter)) ++
            default(x) ++ recHanoi(ys, Move(move.inter, move.from, move.to))
        }
      }
    }

    recHanoi(data, Move(1, 2, 3))
  }

  val result = hanoi(List("A", "B", "C").reverse)
  println(s"Moves: ${result.mkString(", ")}")
}