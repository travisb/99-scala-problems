package im.trav.ninetyninescalaproblems.ch01


object Solutions {
  /*
   * Problem 1 (p01)
   *
   * Find the last element of a list
   * scala> last(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 8
   * 
   */
  def lastElementOfList[A](ls:List[A]):A = {
    ls match {
      case x :: Nil => x
      case x :: xs => lastElementOfList(xs)
      case _ => throw new Exception
    }
  }

  def lastButOneElementOfList[A](as:List[A]):A = {
    as match {
      case x :: _ :: Nil => x 
      case x :: xs => lastButOneElementOfList(xs)
      case _ => throw new Exception
    }
  }

  def lastButNthElementOfList(as:List[Any], n:Int):Any= {
      (as, n) match {
        case (x :: xs, 0)  => x
        case (x :: xs, _) => lastButNthElementOfList(xs, n-1)
        case _ => throw new NoSuchElementException
      }
  }

  def numberOfElements[A](as:List[A], count:Int = 0):Int = {
    as match {
      case (x :: xs)  => numberOfElements(xs, count + 1)
      case  Nil => count 
    }
  }
  
  /*
   *
   * P05 (*) Reverse a list
   *
   */
  def reverseList[A](as:List[A]):List[A] = {
    def go(ass:List[A], ret:List[A]=List()):List[A] = {
      ass match {
        case Nil => ret 
        case (x :: xs) => go(xs, x +: ret)
      }
    }
    go(as, List())
  }

  def isListPalindrome[A](xs:List[A]):Boolean = {
    xs.reverse == xs  
  }

  def isListPalindromeSillySolution[A](xs:List[A]):Boolean = {
    xs.zip(xs.reverse).map(x => x._1 == x._2).foldLeft(true)(_ && _)
  }

  /**
   *
   * p07 (**) Flatten a nested list structure
   *
   **/

  /*
   * using a var and a for loop
   */
  def flatten(as: List[Any]): List[Any] = {
    var result = List[Any]()
    for( x <- as) { 
      x match {
        case x:List[Any] =>  result = result ++: flatten(x)
        case _ => result = result :+ x 
      }
    }
    result
  }

  //TODO: for with yield ?

  /*
   * With more complex pattern matching than we've seen so far
   */
  def flattenPrime(as: List[Any]): List[Any] = {
    as match {
      case Nil => Nil
      case (x :: xs) :: xss => x :: flatten(xs) :: flatten(xss)
      case x :: xs => x :: flatten(xs)
    }
  }
  
  /**
   *
   * P08 (**) Eliminate consecutive duplicates of list elements
   *
   **/
  def eliminateConsecutiveDuplicates[A](as:List[A]):List[A] = {
    def go[A](as:List[A], prev:A):List[A] = {
      (as, prev) match {
        case (x :: Nil, p) => x :: Nil
        case (x :: xs, p) if p != x => x :: go(xs, x)
        case (x :: xs, p) if p == x => go(xs, x)
      }
    }
    as.head :: go(as, as.head)
  }
  //MAYBE TODO: can we do this with List#takeWhile?
  //
  //def eliminateConsecutiveDuplicatesPrime[A](as
  //

  /**
   *
   * P09 (**) Pack consecutive duplicates of list elements into sublists
   * If a list contains repeated elements they should be placed in separate sublists. 
   *
   * Example: 
   * In: List(1,1,1,2,2,3)
   * Out: List(List(1,1,1), List(2,2), List(3))
   * 
   **/
  def pack[A](as:List[A]):List[List[A]] = {
    as.takeWhile(_ == as.head) :: pack(as.dropWhile(_ == as.head))
  }

  def packWithSpan[A](as:List[A]):List[List[A]] = {
    as match {
      case (x :: xs) => {
        val things = xs.span(_ == x)
        x +: things._1 :: packWithSpan(things._2)
      }
      case Nil => Nil
    }
  }
 
  /**
   *
   * P10 (*) Run-length encoding of a list
   * Use the result of problem 1.09 to implement the so-called run-length encoding 
   * data compression method. Consecutive duplicates of elements are encoded as terms 
   * [N,E] where N is the number of duplicates of the element E.
   *
   **/
  def encode[A](as:List[A]):List[(Int, A)] = {
    pack(as).map(x => (x.size, x.head))
  }
  
  /**
   *
   * P11 (*) Modified run-length encoding
   * Modify the result of problem 1.10 in such a way that if an element has no duplicates 
   * it is simply copied into the result list. Only elements with duplicates are 
   * transferred as [N,E] terms.
   *
   **/
  def modifiedEncode[A](as:List[A]):List[Any] = {
    encode(as).map(e => {
      e match {
        case (1, thing) => thing
        case (num, thing) => (num, thing)
      }
    })
  }


  /**
   *
   * P12 (**) Decode a run-length encoded list Given a run-length code list generated as 
   * specified in problem 1.11. Construct its uncompressed version.
   *
   **/
 def decodeModifiedEncode(as:List[Any]):List[Any] = {
   as match {
     case (howMany:Int, thing) :: xs => List.fill(howMany)(thing) ++ decodeModifiedEncode(xs)
     case thing :: xs => thing +: decodeModifiedEncode(xs)
     case Nil => Nil
   }
  }

 /**
  *
  * P13 (**) Run-length encoding of a list (direct solution)
  * Implement the so-called run-length encoding data compression method directly. 
  * I.e. don't explicitly create the sublists containing the duplicates, as in problem 
  * 1.09, but only count them
  *
  **/
  def directRunLengthEncode[A](as:List[A]):List[Any] = {
    as match {
      case x :: Nil => x :: Nil
      case (x :: xs) if xs.head != x => x +: directRunLengthEncode(xs)
      case (x :: xs) => { 
        val newxs = xs.takeWhile( _ == x)
        val rest = xs.dropWhile(_ == x)
        (newxs.size + 1, x) :: directRunLengthEncode(rest)
      }
    }
  }

  /**
   * *
   * * @param as
   * * @return
   *
   * P14 (*) Duplicate the elements of a list
   *
   * Example: 
   * In: List(1,2,3)
   * Out: List(1,1,2,2,3,3)
   *
   **/
  def dupEachElement[A](as:List[A]):List[A] = {
    as match {
      case Nil => Nil
      case (x :: Nil) => x :: x :: Nil
      case (x :: xs) => x :: x :: dupEachElement(xs)
    }
  }

  // TODO: Trav problem: as in P14, but if a given element appears twice in a row 
  // in the original list don't duplicate 
  // Example:
  // In: List(1,2,2,3)
  // Out: List(1,1,2,2,3,3)

  /**
   *
   * @param as
   * @param times
   * @return
   *
   * P15 (**) Duplicate the elements of a list a given number of times
   *
   **/
  def dupElementsNTimes[A](as:List[A], times:Int):List[A] = {
    as match {
      case Nil => Nil
      case (x :: Nil) => List.fill(times)(x)
      case (x :: xs) => List.fill(times)(x) ++ dupElementsNTimes(xs, times)
    }
  }

  def dupElementsNTimesPrime[A](as:List[A], times:Int):List[A] = {
    as.flatMap (a => List.fill(times)(a) )
  }

  /**
   * P16 (**) Drop every N'th element from a list
   * 
   * @param as
   * @param n
   * @return
   *
   */
  def dropNth[A](as:List[A], n:Int=0):List[A] = {
    def dropNthWithOrig(as:List[A], n:Int=0, origN:Int):List[A] = {
      (as, n) match {
        case (x :: Nil, 0) => Nil
        case (x :: Nil, _) => x :: Nil
        case (x :: xs, 0) =>  dropNthWithOrig(xs, origN, origN) 
        case (x :: xs, n) if n != 0 => x :: dropNthWithOrig(xs, n-1, origN)
      }
    }
    dropNthWithOrig(as, n, n)
  }

  /**
   *
   * P17 (*) Split a list into two parts; the length of the first part is given
   *
   * Example: 
   * In: List(1,2,3,4,5,6,7,8,9,10), 3
   * Out: List(List(1,2,3), List(4,5,6,7,8,9,10))
   *
   **/
  def splitListAt[A](as:List[A], splitAt:Int): List[List[A]] = {
    List(as.take(splitAt), as.drop(splitAt))
  }

  /**
   *
   * P18 (**) Extract a slice from a list
   *
   * TODO: NOTE: I don't like this solution because the arithmetic on `start` and `end`
   * isn't intuitive 
   **/
  def extractSlice[A](as:List[A], start:Int, end:Int):List[A] = {
    (as, start, end)  match {
      case (x :: xs,  s, e) if s > 1 => extractSlice(xs, s-1, e - 1)
      case (x :: xs,  s, e) if e > 0 => x :: extractSlice(xs, s, e - 1)
      case (_, _, _) => Nil
    }
  }
  
  /**
    * P19 (**) Rotate a list N places to the left
    *
    * @param as
    * @param n
    * @return
    */
  def rotateN[A](as:List[A], n:Int):List[A] = {
    (as, n) match {
      case (Nil, _ )     => Nil
      case (x :: xs, 0)  => x :: xs
      case (x :: xs, r)  => rotateN(xs :+ x, r-1)
    }
  }
  
  /**
    * 
    * P20 (*) Remove the K'th element from a list
    *
    * @param as
    * @param k
    * @return
    */
  def removeKthElement[A](as:List[A], k:Int):List[A] = {
    (as, k) match {
      case (x :: xs, 0) => xs 
      case (x :: xs, n)  if n != 0 => x :: removeKthElement(xs, n-1)
    }
  }

    
 /**
  *
  * P21 (*) Insert an element at a given position into a list 
   *
   * @param as
   * @param k
   * @return
   */ 
  def insertAtKthPosition[A](as:List[A], elem:A, k:Int): List[A] = {
    (as, k) match { 
      case (x :: xs, 1) => x :: (elem +: xs)
      case (x :: xs, n) => x :: insertAtKthPosition(xs, elem, k-1)
      case (Nil, _) => Nil
    } 
  }


  /**
    * P22 (*) Create a list containing all integers within a given range
    *
    * This is a silly solution, could do just `(start to end).toList
    * or recurse on createIntegerRange(start-1, end) but im bored within
    * the pattern matching rn
    *
    * @param start
    * @param end
    * @return
    */
  def createIntegerRange(start:Int, end:Int):List[Int] = {
    (for( num <- (start to end)) yield num).toList
      
  }

 /**
  * 
  * P23 (**) Extract a given number of randomly selected elements from a list
  * Hint: Use the built-in scala.util.Random and the result of problem P20
  *
  * @param as
  * @return
  *
  */
  import scala.util.Random 

  def extractKRandomElements[A](as:List[A], k:Int):List[A] = {
      (for(num <- (0 to k-1)) yield as(Random.nextInt(as.size))).toList
  }
 
  /**
    * 
    * P24 (*) Lotto: Draw N different random numbers from the set 1..M
    *
    * @param howMany
    * @param range
    * @return
    * 
    */
  def lottoDrawN(howMany:Int, range:(Int, Int)):List[Int] = {
    extractKRandomElements((range._1 to range._2).toList, howMany)
  }
  
  /**
   *
   * P25 (*) Generate a random permutation of the elements of a list
   *
   * Hint: Use the solution of problem P23
   *
   * Reference: https://scalasolutions.wordpress.com/page/3/
   * TODO: Come back to this with a clear head
   *
   * @param as
   */

  def removeAt[T](pos: Int, list: List[T]) : (List[T], T) = {
    val resList = list.zipWithIndex.foldLeft(List[T]()) { (acc, tuple) =>
        tuple match {
          case (item, idx) if idx == pos => acc
          case (item, idx) => acc ::: List(item)
        }
      }
      (resList, list(pos))
  }
  
  def randomSelect[T](count: Int, list: List[T]) : List[T] = {
     if (count <= 0) {
       List[T]()
     } else {
       val index = (Math.random * list.length).toInt
       val (shorterList, item) = removeAt(index, list)
       item :: randomSelect(count - 1, shorterList)
     }
  }



}
