import org.scalatest._
import s99.s99

class S99Spec extends FeatureSpec with GivenWhenThen with ShouldMatchers {
  val s99 = new s99

  feature("P01") {
    scenario("p01 last") {
      val ls = List(1, 1, 2, 3, 5, 8)
      val result = s99.last(ls)
      then("Find the last element of a list")
      result should be === 8
    }
  }

  feature("P02") {
    scenario("p02 penultimate") {
      val ls = List(1, 1, 2, 3, 5, 8)
      val result = s99.penultimate(ls)
      then("Find the last but one element of a list.")
      result should be === 5
    }
  }

  feature("P03") {
    scenario("p03 nth") {
      val ls = List(1, 1, 2, 3, 5, 8)
      val result = s99.nth(2, ls)
      then("Find the last element of a list")
      result should be === 2
    }
  }

  feature("P04") {
    scenario("p04 length") {
      val ls = List(1, 1, 2, 3, 5, 8)
      val result = s99.length(ls)
      then("Find the last element of a list")
      result should be === 6
    }
  }

  feature("P05") {
    scenario("p05 reverse") {
      val ls = List(1, 1, 2, 3, 5, 8)
      val result = s99.reverse(ls)
      then("Reverse a list")
      result should be === List(8, 5, 3, 2, 1, 1)
    }
  }

  feature("P06") {
    scenario("p06 isPalindrome") {
      val ls = List(1, 2, 3, 2, 1)
      val result = s99.isPalindrome(ls)
      then("Find out whether a list is a palindrome.")
      result should be === true
    }
  }

  feature("P07") {
    scenario("p07 flatten") {
      val ls = List(List(1, 1), 2, List(3, List(5, 8)))
      val result = s99.flatten(ls)
      then("Flatten a nested list structure.")
      result should be === List(1, 1, 2, 3, 5, 8)
    }
  }

  feature("P08") {
    scenario("p08 compress") {
      val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val result = s99.compress(ls)
      then("Eliminate consecutive duplicates of list elements.")
      result should be === List('a, 'b, 'c, 'a, 'd, 'e)
    }
  }

  feature("P09") {
    scenario("p09 pack") {
      val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val result = s99.pack(ls)
      then("Pack consecutive duplicates of list elements into sublists.")
      result should be ===  List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    }
  }

  feature("P10") {
    scenario("p10 encode") {
      val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val result = s99.encode(ls)
      then("Run-length encoding of a list")
      result should be ===  List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    }
  }

  feature("P11") {
    scenario("p11 encodeModified") {
      val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val result = s99.encodeModified(ls)
      then("Modified run-length encoding")
      result should be ===  List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    }
  }

  feature("P12") {
    scenario("p12 decode") {
      val ls = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
      val result = s99.decode(ls)
      then("Decode a run-length encoded list")
      result should be ===  List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    }
  }

  feature("P13") {
    scenario("p13 encodeDirecet") {
      val ls = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val result = s99.encodeDirect(ls)
      then("Run-length encoding of a list (direct solution).")
      result should be ===  List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    }
  }

  feature("P14") {
    scenario("p14 duplicate") {
      val ls = List('a, 'b, 'c, 'c, 'd)
      val result = s99.duplicate(ls)
      then("Duplicate the elements of a list.")
      result should be === List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    }
  }

  feature("P15") {
    scenario("p15 duplicateN") {
      val ls = List('a, 'b, 'c, 'c, 'd)
      val result = s99.duplicateN(3, ls)
      then("Duplicate the elements of a list a given number of times")
      result should be ===  List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    }
  }

  feature("P16") {
    scenario("p16 drop") {
      val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val result = s99.drop(3, ls)
      then("Drop every Nth element from a list.")
      result should be === List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    }
  }

  feature("P17") {
    scenario("p17 split") {
      val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val result = s99.split(3, ls)
      then("Split a list into two parts")
      result should be === (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }
  }

  feature("P18") {
    scenario("p18 slice") {
      val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val result = s99.slice(3, 7, ls)
      then("Extract a slice from a list.")
      result should be === List('d, 'e, 'f, 'g)
    }
  }

  feature("P19") {
    scenario("p19 rotate") {
      val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val result_test1 = s99.rotate(3, ls)
      then("Extract a slice from a list.")
      result_test1 should be === List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
      val result_test2 = s99.rotate(-2, ls)
      result_test2 should be === List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    }
  }

  feature("P20") {
    scenario("p20 removeAt") {
      val ls = List('a, 'b, 'c, 'd)
      val result = s99.removeAt(1, ls)
      then("Remove the Kth element from a list.")
      result should be === (List('a, 'c, 'd), 'b)
    }
  }

  feature("P21") {
    scenario("p21 insertAt") {
      val ls = List('a, 'b, 'c, 'd)
      val result = s99.insertAt('new, 1, ls)
      then("Insert an element at a given position into a list.")
      result should be === List('a, 'new, 'b, 'c, 'd)
    }
  }

  feature("P22") {
    scenario("p22 range") {
      // val ls = List('a, 'b, 'c, 'd)
      val result = s99.range(4, 9)
      then("Create a list containing all integers within a given range.")
      result should be === List(4, 5, 6, 7, 8, 9)
    }
  }

  feature("P23") {
    scenario("p23 randomSelect") {
      val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val result = s99.randomSelect(3, ls)
      given("count: 3, " + ls)
      then("Create a list containing all integers within a given range.")
      result.size should be === 3
      result.filter{
        ls.contains(_)
      }
      result.size should be === 3
      and("" + result)
    }
  }

  feature("P24") {
    scenario("p24 lotto") {
      val result = s99.lotto(6, 49)
      given("count: 6, max: 49")
      then("Lotto: Draw N different random numbers from the set 1..M")
      result.size should be === 6
      result.filter{
        _ < 49
      }
      result.size should be === 6
      and("" + result)
    }
  }

  feature("P25") {
    scenario("p25 randomPermute") {
      val ls = List('a, 'b, 'c, 'd, 'e, 'f)
      val result = s99.randomPermute(ls)
      given("" + ls)
      then("Generate a random permutation of the elements of a list.")
      result.filter{
        ls.contains(_)
      }
      result.size should be === ls.size
      result == ls should be === false
      and("" + result)
    }
  }

  feature("P26") {
    scenario("p26 combination") {
      val ls = List('a, 'b, 'c)
      val result = s99.combinations(2, ls)
      // given("" + ls)
      then("Generate the combinations of K distinct objects chosen from the N elements of a list.")
      result should be === List(List('a, 'b), List('a, 'c), List('b, 'c))
    }
  }

  feature("P27") {
    scenario("p27 group") {
      val countList = List(1, 1, 2)
      val itemList = List('a, 'b, 'c, 'd)
      given("" + countList + ", " + itemList)
      val result = s99.group(countList, itemList)
      then("Group the elements of a set into disjoint subsets.")
      result.contains(List(List('a), List('b), List('c, 'd))) should be === true
      and("" + result)
    }
  }

  feature("P28a") {
    scenario("p28 lsort") {
      val targetList = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
      given("" + targetList)
      val result = s99.lsort(targetList)
      then("Sorting a list of lists according to length of sublists.")
      result == List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)) should be === true
      and("" + result)
    }
  }

  feature("P28b") {
    scenario("p28 lsortFreq") {
      val targetList = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
      given("" + targetList)
      val result = s99.lsortFreq(targetList)
      then("this time the objective is to sort the elements according to their length frequency")
      result == List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)) should be === true
      and("" + result)
    }
  }
}
