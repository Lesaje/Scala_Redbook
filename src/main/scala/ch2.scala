import scala.annotation.tailrec

object ch2 {

  def fib (n: Int): Int = {
    @tailrec
    def loop(a: Int, b: Int, acc: Int): Int = {
      if acc == n then b
      else if acc == n+1 then a
      else loop(b, a+b, acc+1)
    }

    loop(0, 1, 1)
  }

  @tailrec
  def isSorted[A](arr: List[A], ordered: (A, A) => Boolean): Boolean = {
    if arr.tail == null then true
    else if ordered(arr.head, arr.tail.head) then isSorted(arr.tail, ordered)
    else false
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}
