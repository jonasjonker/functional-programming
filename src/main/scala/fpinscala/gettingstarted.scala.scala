package fpinscala.gettingstarted

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, acc * n)

    go(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if (n <= 0) prev
      else go(n - 1, cur, prev + cur)

    go(n, 0, 1)
  }

  private def debug(name: String, n: Int, f: Int => Int): Unit = {
    println("%12s => %d".format("%s(%d)".format(name, n), f(n)))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) go(n + 1)
      else false

    go(0)
  }


  def main(args: Array[String]): Unit = {
    debug("abs", -42, abs)
    debug("factorial", 7, factorial)
    debug("fibonaci", 42, fibonacci)
    println(isSorted(Array(1,2,3), (x: Int, y: Int) => x <= y))
    println(isSorted(Array(2,2,3), (x: Int, y: Int) => x <= y))
    println(isSorted(Array(3,2,3), (x: Int, y: Int) => x <= y))
  }

}
