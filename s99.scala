package s99

class s99 {
  // 1
  def last[A](list: List[A]): A = {
    list match {
      case x :: Nil => x
      case _ :: tail => last(tail)
      case _ => throw new NoSuchElementException
    }
  }

  // 2
  def penultimate(list: List[Int]): Int = {
    list match {
      case x :: _ :: Nil => x
      case x :: tail => penultimate(tail)
      case _ => throw new NoSuchElementException
    }
  }


  // 3
  def nth(n:Int, list:List[Int]): Int = {
    return list(n)
  }

  // 4
  def length[A](list: List[A]): Int = {
    list match {
      case Nil => 0
      case x :: tail => 1 + length(tail)
      case _ => throw new NoSuchElementException
    }
  }

  // 5
  def reverse[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case x :: tail => reverse(tail) ++ List(x)
    }
  }

  // 6
  def isPalindrome[A](list: List[A]): Boolean = {
    list == list.reverse
  }


  // 7
  def flatten(list: List[Any]): List[Any] = {
    list flatMap {
      case x: List[Any] => flatten(x)
      case e: Int => List(e)
      case _ => Nil
    }
  }

  // 8
  def compress(list: List[Any]): List[Any] = {
    list match {
      case Nil => Nil
      case h :: tail => h :: compress(tail.dropWhile(_ == h))
    }
  }

  // 9
  def pack(list: List[Any]): List[List[Any]] = {
    list match {
      case Nil => Nil
      case h :: Nil => List(List(h))
      case h :: tail => {
        val (take, drop) = tail.span(_==h)
        (h :: take) :: pack(drop)
      }
    }
  }

  // 10
  def encode(list: List[Any]): List[(Int, Any)] = {
    list match {
      case Nil => Nil
      case h :: Nil => List((1, h))
      case h :: tail => {
        val (take, drop) = tail.span(_ == h)
        (take.size + 1, h) :: encode(drop)
      }
    }
  }

  // 11
  def encodeModified(list: List[Any]): List[Any] = {
    list match {
      case Nil => Nil
      case h :: Nil => List(h)
      case h :: tail => {
        val (take, drop) = tail.span(_ == h)
        take match {
          case Nil => h :: encodeModified(drop)
          case h :: tail => {
            (take.size + 1, h) :: encodeModified(drop)
          }
        }
      }
    }
  }

  // 12
  def decode(list: List[(Int, Any)]): List[Any] = {
    list flatMap {
      item => List.fill(item._1)(item._2)
    }
  }

  // 13
  def encodeDirect(list: List[Any]): List[(Int, Any)] = {
    list match {
      case Nil => Nil
      case h => {
        val (take, drop) = h.span(_ == h.head)
        (take.size, take.head) :: encodeDirect(drop)
      }
    }
  }

  // 14
  def duplicate(list: List[Any]): List[Any] = {
    list flatMap {
      item => List(item, item)
    }
  }

  // 15
  def duplicateN(n: Int, list: List[Any]): List[Any] = {
    list flatMap {
      item => List.fill(n)(item)
    }
  }

  // 16
  def _drop(n: Int, list: List[Any]): List[Any] = {
    // filterとmapをまとめたい
    list.zipWithIndex.filter {
      index_item => index_item._2 % 3 != 2
    }.map(_._1)
  }
  def drop(n: Int, list: List[Any]): List[Any] = {
    def dropRecursive(cutIndex: Int, ls: List[Any]): List[Any] = {
      (cutIndex, ls) match {
        case (_, Nil) => Nil
        case (1, h :: tail) => dropRecursive(n, tail)
        case (_, h :: tail) => h :: dropRecursive(cutIndex - 1, tail)
      }
    }
    dropRecursive(n, list)
  }

  // 17
  def split(n: Int, list: List[Any]): (List[Any], List[Any]) = {
    // partitionとmapを組み合わせたい
    val (forward, backward) = list.zipWithIndex.partition {
      index_item => index_item._2 < n
    }
    (forward.map(_._1), backward.map(_._1))
  }

  // 18
  def slice(i: Int, j: Int, list: List[Any]): List[Any] = {
    list.take(j).zipWithIndex.filter {
      index_item => index_item._2 >= i
    }.map(_._1)
  }

  // 19
  def rotate(n: Int, list: List[Any]): List[Any] = {
    n match {
      case 0 => list
      case n if n > 0 => {
        list.drop(n) ::: list.take(n)
      }
      case n if n < 0 => {
        rotate(list.size + n, list)
      }
      case _ => Nil
    }
  }

  // 20
  def removeAt(n: Int, list: List[Any]): (List[Any], Any) = {
    val (forward, backward) = list.splitAt(n)
    (forward ::: backward.drop(1), backward.head)
  }

  // 21
  def insertAt(item: Any, n: Int, list: List[Any]): List[Any] = {
    val (forward, backward) = list.splitAt(n)
    forward ::: item :: backward
  }

  // 22
  def range(i: Int, j: Int): List[Int] = {
    (i, j) match {
      case (i, j) if i > j => Nil
      case _ => i :: range(i + 1, j)
    }
  }

  import scala.util.Random
  val r = new Random
  // 23
  def randomSelect(n: Int, list: List[Any]): List[Any] = {
    list match{
      case list if list.size == n => list
      case _ => {
        val removeIndex = r.nextInt(list.size)
        randomSelect(n, removeAt(removeIndex, list)._1)
      }
    }
  }

  // 24
  def lotto(i: Int, j: Int): List[Int] = {
    def lottoRecursive(n: Int): List[Int] = {
      n match {
        case 0 => Nil
        case _ => {
          val num = r.nextInt(j)
          num :: lottoRecursive(n - 1)
        }
      }
    }
    lottoRecursive(i)
  }

  // 25
  def randomPermute(list: List[Any]): List[Any] = {
    def randomPermuteRecursive(ls: List[Any]): List[Any] = {
      ls match {
        case Nil => Nil
        case _ => {
          val randomIndex = r.nextInt(ls.size)
          val (removedList, removedItem) = removeAt(randomIndex, ls)
          removedItem :: randomPermuteRecursive(removedList)
        }
      }
    }
    randomPermuteRecursive(list)
  }

  // 26
  // def _combinations[A](n: Int, list: List[A]): List[List[A]] = {
  //   def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] = {
  //     ls match {
  //       case Nil => Nil
  //       case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
  //     }
  //   }
  //   def combinationsRecursive[A](n: Int, ls: List[A]): List[List[A]] = {
  //     if (n == 0) List(Nil)
  //     else flatMapSublists(ls) { sl =>
  //       combinations(n - 1, sl.tail) map {sl.head :: _}
  //     }
  //   }
  //   combinationsRecursive(n, list)
  // }

  // 26
  def combinations(n: Int, list: List[Any]): List[List[Any]] = {
    def combinationsRecursive(n: Int, selectedList: List[Any], restList: List[Any]): List[List[Any]] = {
      (n, restList) match{
        // 先頭の要素から順番に追加しているためreverse
        case (0, _) => List(selectedList.reverse)
        case (_, Nil) => Nil
        case (_, h :: tail) => {
          // hを選んだ場合
          combinationsRecursive(n - 1, h :: selectedList, tail) :::
          // hを選ばなかった場合
          combinationsRecursive(n, selectedList, tail)
        }
      }
    }
    combinationsRecursive(n, Nil, list)
  }

  // 27
  def group(countList: List[Int], itemList: List[Any]): List[List[List[Any]]] = {
    countList match {
      case Nil => List(Nil)
      case n :: tail => {
        // itemListからcountListの先頭の要素数分combinationsで選択する
        // 生成されたList[List[Any]]の各要素ごとにitemListから取り除き、
        // countListの次の要素に応じてgroupを再帰させる
        // 再帰させた結果を既に選んだリストの後に付け加えていく
        combinations(n, itemList) flatMap {
          selectedList => {
            group(tail, itemList.diff(selectedList)).map(selectedList :: _)
          }
        }
      }
    }
  }

  // 28
  def lsort[A](ls: List[List[A]]): List[List[A]] = {
    // 組み込み関数
    ls.sortBy { _.length }
  }
  def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
    val lengths = ls map { _.length }
    ls.sortBy { i => lengths.count(j => i.length == j)}
  }
}
