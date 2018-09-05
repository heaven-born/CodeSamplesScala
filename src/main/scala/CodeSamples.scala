
object CodeSamples {

  def main(args: Array[String]): Unit = {


    // P3 Find the Kth element of a list.
    def nth[A](k: Int, ls: List[A]): A = ls match {
      case Nil => throw new IllegalArgumentException
      case n :: _ if k == 1 => n
      case n :: tail => nth(k - 1, tail)
    }


    //P4 Find the number of elements of a list.
    def length[A](l: List[A], lg: Int = 0): Int = l match {
      case Nil => lg
      case _ :: tail => length(tail, lg + 1)
    }

    def lengthFunctional[A](l: List[A]) = l.foldLeft(0) { (n, _) => n + 1 }


    //P5 Reverse a list.
    def reverse[A](ints: List[A]): List[A] = {
      def reverseInternal(in: List[A], out: List[A]): List[A] = in match {
        case Nil => out
        case _ => reverseInternal(in.tail, in.head :: out)
      }

      reverseInternal(ints, List())
    }

    def reverseFunctional[A](ints: List[A]): List[A] = ints.foldLeft(List[A]()) { (l, elem) => elem :: l }

    //P06 (*) Find out whether a list is a palindrome.

    def isPalindromeFast(ls: List[Int]) = ls.drop((ls.length + 1) / 2) == reverse(ls.dropRight((ls.length + 1) / 2))

    def isPalindromeElegant(ls: List[Int]) = ls == reverse(ls)


    //P07 (**) Flatten a nested list structure.

    def flatten(list: List[Any]): List[Any] = list match {
      case (l: List[Any]) :: tail => flatten(l) ::: flatten(tail)
      case o :: tail => o :: flatten(tail)
      case Nil => Nil
    }

    //P08 (**) Eliminate consecutive duplicates of list elements.

    def compress(symbols: List[Symbol]): List[Symbol] = symbols.foldRight(List[Symbol]()) { (s, ls) =>
      if (ls.isEmpty || ls.head != s) s :: ls else ls
    }

    //P09 (**) Pack consecutive duplicates of list elements into sublists.

    def pack(sym: List[Symbol]): List[List[Symbol]] = sym.span(_ == sym.head) match {
      case (x, Nil) => List(x)
      case (x, y) => x :: pack(y)
    }

    //P10 (*) Run-length encoding of a list.

    def encode(symbols: List[Symbol]): List[(Int, Symbol)] = pack(symbols).map(x => (x.length, x.head))


    //P11 (*) Modified run-length encoding.

    def encodeModified(symbols: List[Symbol]): List[Any] = encode(symbols).map {
      case x if x._1 == 1 => x._2
      case x => x
    }

    //P12 (**) Decode a run-length encoded list.

    def decode(ls: List[(Int, Symbol)]): List[Symbol] = ls.flatMap(x => List.tabulate(x._1)(_ => x._2))

    //P13 (**) Run-length encoding of a list (direct solution).


    def encodeDirect[A](sym: List[A]): List[(Int, A)] = {
      def typle(x:List[A]) = (x.length, x.head)
      sym.span(_ == sym.head) match {
        case (x, Nil) => List(typle(x))
        case (x, y) => typle(x) :: encodeDirect(y)
      }
    }

    // P14 (*) Duplicate the elements of a list.

    def duplicate(symbols: List[Symbol]) = symbols.flatMap(x=>List(x,x))

    //P15 (**) Duplicate the elements of a list a given number of times.

    def duplicateN(n:Int, symbols: List[Symbol]) = symbols.flatMap(x=>List.tabulate(n)(_ => x))

    //P16 (**) Drop every Nth element from a list.

    def drop(n:Int, ls: List[Symbol]):List[Symbol] =  ls.splitAt(n) match {
      case (a,Nil) => a
      case (a,b) => a.init ::: drop(n,b)
    }

    //P17 (*) Split a list into two parts.

    def splitBuiltIn[A](i: Int, symbols: List[A]):(List[A],List[A]) = symbols.splitAt(i)

    def split[A](i: Int, symbols: List[A]):(List[A],List[A]) = symbols match {
      case x :: tail if i > 0 => Some(split(i - 1, tail)).map(y=>(x :: y._1, y._2)).get
      case x :: tail => (List(x),tail)
    }

    //P18 (**) Extract a slice from a list.

    def sliceBuitin(i: Int, i1: Int, symbols: List[Symbol]) =symbols.slice(i,i1)

    def sliceTailRecursive(i: Int, i1: Int, symbols: List[Symbol]) = {
      def sliceCollect(n: Int, c: Int, symbols: List[Symbol], acc: List[Symbol]):List[Symbol] = symbols match {
        case Nil => Nil
        case _ :: tail if n > 0 => sliceCollect(n-1, c, tail, acc)
        case x :: tail if c > 0 => sliceCollect(n, c-1, tail, x :: acc)
        case _ => acc.reverse
      }

      sliceCollect(i,i1,symbols,List())
    }

    //P19 (**) Rotate a list N places to the left.

    def rotate[A](i: Int, symbols: List[A]): List[A] = {
      val iAdjusted = if (i < 0) symbols.length + i else i
      val sp: (List[A], List[A]) = symbols.splitAt(iAdjusted)
      sp._2:::sp._1
    }

    //P20 (*) Remove the Kth element from a list.

    def removeAt(i: Int, symbols: List[Symbol]):(List[Symbol],Symbol) = {
      if (symbols.length<=i) throw new IllegalArgumentException
      val sp = symbols.splitAt(i+1)
      (sp._1.init:::sp._2, sp._1.last)
    }


    //P21 (*) Insert an element at a given position into a list.

    def insertAt[A](symbol: A, i: Int, symbols: List[A]):List[A] = {
      val sp = symbols.splitAt(i)
      sp._1:::symbol::sp._2
    }

    //P22 (*) Create a list containing all integers within a given range.

    def rangeBuiltin(i: Int, i1: Int): List[Int] = (i to i1).toList

    def range(start: Int, stop: Int): List[Int] = start match {
      case _ if start > stop => throw new IllegalArgumentException
      case i if i < stop => i :: range(i + 1,stop)
      case _ => List(stop)
    }

    def rangeTailRecursive(start: Int, stop: Int): List[Int] = {
      if (start>stop) throw new IllegalArgumentException
      def rangeInternal(start: Int, acc: List[Int]):List[Int] = {
        if (start==stop) stop::acc else rangeInternal(start+1,start::acc)
      }
      rangeInternal(start,List()).reverse
    }


    //P23 (**) Extract a given number of randomly selected elements from a list.

    def randomSelect(i: Int, symbols: List[Symbol]): List[Symbol] = scala.util.Random
      .shuffle(symbols.indices.toList)
      .take(i)
      .map(x=>removeAt(x,symbols))
      .map(_._2).toList


    //P24 (*) Lotto: Draw N different random numbers from the set 1..M.


    def lotto(i: Int, i1: Int): List[Int] = scala.util.Random.shuffle(1 to i1).take(i).toList

    //P25 (*) Generate a random permutation of the elements of a list.

    def randomPermute(symbols: List[Symbol]): List[Symbol] = randomSelect(symbols.length,symbols)

    //P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.

    def combinationsBuiltin(i: Int, symbols: List[Symbol]): List[List[Symbol]] = symbols.combinations(i).toList

    def combinationsRecursive(i: Int, symbols: List[Symbol]): List[List[Symbol]] = symbols match {
      case ls if i > ls.length => Nil
      case ls if i ==1 => ls.map(List(_))
      case h :: tail =>  combinationsRecursive(i-1,tail).map(h :: _) ::: combinationsRecursive(i,tail)
    }


    //P27 (**) Group the elements of a set into disjoint subsets.

    def group3(lst: List[String]): List[List[List[String]]]  = {
       for { comb2 <- lst.combinations(2)
             comb3 <- (lst diff comb2).combinations(3)
             comb4 <- (lst diff comb3 diff comb2).combinations(4)
       } yield List(comb2,comb3,comb4)
    }.toList

    def group(groups: List[Int], people: List[String]): List[List[List[String]]]  = {
      // --- TBD ---
      List()
    }

    //println(group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")))

    //P28 (**) Sorting a list of lists according to length of sublists.

    def lsort(list: List[List[Symbol]]):List[List[Symbol]] = list.sortWith(_.length<_.length)


    def lsortFreq(list: List[List[Symbol]]):List[List[Symbol]] = list
      .groupBy(_.length)
      .values
      .toList
      .map(t=>(t.length,t))
      .sortBy(_._1)
      .flatMap(_._2)


  }



}
