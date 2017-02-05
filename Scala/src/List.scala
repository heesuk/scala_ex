sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
   def sums(ints: List[Int]) : Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x+sums(xs)
  }
  
  def product(ds:List[Double]):Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def applys[A](as: A*):List[A] = 
    if(as.isEmpty) Nil
    else Cons(as.head, applys(as.tail: _*))
}