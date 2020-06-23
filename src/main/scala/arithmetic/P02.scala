package im.trav.ninetyninescalaproblems.ch02
import scala.math

object Solutions {
  /**
   *
   * P31 (**) Determine whether a given integer number is prime.
   *
   */
  def isPrimeNumber(num:Int):Boolean = {
    val numRoot= math.sqrt(num).toInt
    val factors = for(t <- (2 to numRoot) if num % t == 0) yield t

    if (factors.nonEmpty) false else true

  }

}

