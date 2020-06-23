//  def isListPalindromeFunSolution[A](xs:List[A]):Boolean = {
//
//    val revXs = xs.reverse 
    
 //   true

//}

//  isListPalindromeFunSolution(List(1,2,1))
//

//val xs = List(1,1,2,2,1,1)
//val revXs = xs.reverse

//xs.zip(revXs).reduce(
//
//def foo[A >: Equals](xs:List[A]):Boolean = {
 // xs.reduce(_ == _)
//}
//foo(List(1,1,1))
//
//xs.zip(xs.reverse)
//val ys =  List((1, 1), (1, 1), (2, 2), (2, 2), (1, 1), (1, 1))
//ys.map(x => x._1 == x._2).foldLeft(true)(_ == _)
//xs.zip(xs.reverse).map(x => x._1 == x._2).foldLeft(true)(_ && _)

  //.foldLeft(true)(_ ==  _)
//def myThing[A](xs:List[A], op:(A, A) => Boolean)
/*  def flatten(as: List[Any]): List[Any] = {
    var result = List[Any]()
    for( x <- as) { 
      x match {
        case x:List[Any] =>  result = result ++: flatten(x)
        case _ => result = result :+ x 
      }
    }
    result
  }

  flatten(List(1,List(2,3,List(4))))


  def flattenPrime(as: List[Any]): List[Any] = {
    as match {
      case Nil => Nil
      case (x :: xs) :: xss => x :: flatten(xs) :: flatten(xss)
      case x :: xs => x :: flatten(xs)
    }
}

flattenPrime(List(1,List(2,3,List(4))))
*/
def lastButNthElementOfList(as:List[Any], n:Int):Any= {
    (as, n) match {
      case (x :: xs, 0)  => x
      case (x :: xs, _) => lastButNthElementOfList(xs, n-1)
      case _ => throw new NoSuchElementException
    }
}
val f = 1
val count = 4
val list = List(1,2,3,4,5)

lastButNthElementOfList(list, count)
list.take(count+1).last
/*
def testIfInCase[A](as:List[A], n:Int):A = {
  (as, n) match {
    case (x :: xs, 1) => x
    case (x :: xs, _) =>  xs.last
  }
}
*/
//testIfInCase(List(6,2,3), 1)

List(1,2,3).tail

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
val reps = List(1,1,1,2,2,3)
eliminateConsecutiveDuplicates(reps)

def pack[A](as:List[A]):List[List[A]] = {
  if (as == Nil) {
    return List[List[A]]()
  }
 as.takeWhile(_ == as.head) :: pack(as.dropWhile(_ == as.head))
}

pack(reps)

reps.span(_ == 1)
reps(5)
List(4).span( _ == 4)
  def packWithSpan[A](as:List[A]):List[List[A]] = {
    as match {
      case (x :: xs) => {
        val things = xs.span(_ == x)
        x +: things._1 :: packWithSpan(things._2)
      }
      case Nil => Nil
    }
  }
  packWithSpan(reps)

  def encode[A](as:List[A]):List[(Int, A)] = {
    pack(as).map(x => (x.size, x.head))
  }

  def modifiedEncode[A](as:List[A]):List[Any] = {
    val eas = encode(as)
    eas.map(e => {
      e match {
        case (1, thing) => thing
        case (num, thing) => (num, thing)
      }
    })
  }

  List.fill(3)('a')


   def decodeModifiedEncode(as:List[Any]):List[Any] = {
     as match {
       case (howMany:Int, thing) :: xs => List.fill(howMany)(thing) ++ decodeModifiedEncode(xs)
       case thing :: xs => thing +: decodeModifiedEncode(xs)
       case Nil => Nil
     }
    }

decodeModifiedEncode(modifiedEncode(reps))

reps.takeWhile(_ == 9).size

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
  directRunLengthEncode(reps)

  1 :: 1 :: Nil
  
  def dupEachElement[A](as:List[A]):List[A] = {
    as match {
      case Nil => Nil
      case (x :: Nil) => x :: x :: Nil
      case (x :: xs) => x :: x :: dupEachElement(xs)
    }
  }

  dupEachElement(List(1,2,3))


  List(1,2,3) ++ List(4,5,6)
  List.fill(3)('a')

  
  def dupElementsNTimes[A](as:List[A], times:Int):List[A] = {
    as match {
      case Nil => Nil
      case (x :: Nil) => List.fill(times)(x)
      case (x :: xs) => List.fill(times)(x) ++ dupElementsNTimes(xs, times)
    }
  }
  dupElementsNTimes(List(1,2,3), 3)

  

  def dupElementsNTimesPrime[A](as:List[A], times:Int):List[A] = {
    as.flatMap(a => List.fill(times)(a) )
  }

  dupElementsNTimesPrime(List(1,2,3), 3)

  def id[A <: Any](x:A):A = x


 List(List(1,2,3), List(4,5,6)).flatMap(id)


 
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

  dropNth(List(1,2,3,4,5,6), 2)

  reps
  reps.splitAt(3)

  

  def extractSlice[A](as:List[A], start:Int, end:Int):List[A] = {
    (as, start, end)  match {
      case (x :: xs,  s, e) if s > 1 => extractSlice(xs, s-1, e - 1)
      case (x :: xs,  s, e) if e > 0 => x :: extractSlice(xs, s, e - 1)
      case (_, _, _) => Nil
    }
  }

  extractSlice((1 to 10).toList, 4, 6)


  def rotateN[A](as:List[A], n:Int):List[A] = {
    (as, n) match {
      case (Nil, _ )     => Nil
//      case (x :: Nil, r) => x :: Nil 
      case (x :: xs, 0)  => x :: xs
      case (x :: xs, r)  => rotateN(xs :+ x, r-1)
    }

  }
  rotateN((1 to 10).toList, 5)


  def removeKthElement[A](as:List[A], k:Int):List[A] = {
    (as, k) match {
      case (x :: xs, 0) => xs 
      case (x :: xs, n)  if n != 0 => x :: removeKthElement(xs, n-1)
    }

  }
  removeKthElement(List(1,2,3,4,5), 3)

  List(1,2,3,4,5, 6).take(2)

  def insertAtKthPosition[A](as:List[A], elem:A, k:Int): List[A] = {
    (as, k) match { 
      case (x :: xs, 1) => x :: (elem +: xs)
      case (x :: xs, n) => x :: insertAtKthPosition(xs, elem, k-1)
      case (Nil, _) => Nil
    } 
  }
  
99 +:   List(1,2,3,4,5, 6) 

   insertAtKthPosition(List(1,2,3,4,5,6),99, 3) 
   
  def createIntegerRange(start:Int, end:Int):List[Int] = {
    (for(num <- (start to end)) yield num).toList
  }

  createIntegerRange(1,10)


  import scala.util.Random 
  val listForRandomExtraction = (1 to 20).toList
  val  randomInt = Random.nextInt(10)

List(1,2,3,4,5)(1)
def extractKRandomElements[A](as:List[A], k:Int):List[A] = {
      (for(num <- (0 to k-1)) yield as(Random.nextInt(as.size))).toList
}

extractKRandomElements((1 to 20).toList, 20)
extractKRandomElements((1 to 5).toList, (1 to 5).size)

/*


  def lottoDrawN(howMany:Int, range:(Int, Int)):List[Int] = {
    extractKRandomElements((range._1 to range._2).toList, howMany)
  }
lottoDrawN(5, (0, 10))
*/
/*
def randomPermutationOfList[A](as:List[A]) {
  extractKRandomElements( as, as.size)
}

val foorrrr = randomPermutationOfList(List(1,2,3,4,5,6)
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


def randomPermute[T](list: List[T]) = randomSelect(list.length, list)

randomPermute(List(1,2,3,4,5))
