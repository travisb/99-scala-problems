import im.trav.ninetyninescalaproblems.ch01._
import org.scalatest.FunSuite

class SolutionsTest extends org.scalatest.FunSuite {
  test("lastElelmentOfList") {
    val xs = List(1,2,3)
    assert(Solutions.lastElementOfList(xs) === 3)
  }

  test("lastButOneElementOfList") {
    val xs = List(1,2,3)
    assert(Solutions.lastButOneElementOfList(xs) === 2)
  }
  
  test("lastButNthElementOfList") {
    val xs = List(1,2,3)
    assert(Solutions.lastButNthElementOfList(xs, 0) === 1) 
    assert(Solutions.lastButNthElementOfList(xs, 1) === 2)
  }
  
  test("numberOfElements") {
    val xs = List(1,2,3)
    assert(Solutions.numberOfElements(xs) === 3)
  }

  test("reverse list") {
    val xs = List(1,2,3)
    assert(Solutions.reverseList(xs) === List(3,2,1))
  }
  
  test("isListPalindrome") {
    val xs = List(1, 1, 2, 1, 1)
    assert(Solutions.isListPalindromeSillySolution(xs) === true)
    assert(Solutions.isListPalindrome(List(1,2,2,4)) === false)
  }
/*
  test("flatten") {
    val xs = List(1,List(2,3,List(4)))
    assert(Solutions.flatten((xs) === List(1,2,3,4))
  }
*/
  test("flattenPrrime") {
    val xs = List(1,List(2,3,List(4)))
    assert(Solutions.flatten(xs) === List(1,2,3,4))

  }

  test("remove eliminateConsecutiveDuplicates") {
    val xs = List(1,1,1,2,2,3)
    assert(Solutions.eliminateConsecutiveDuplicates(xs) === List(1,2,3))
  }
}
