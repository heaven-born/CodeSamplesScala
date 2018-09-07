object CodeSamplesArithmetic {

  def main(args: Array[String]): Unit = {


    //P31 (**) Determine whether a given integer number is prime.

    implicit class Prime(n:Int) {
      def isPrime: Boolean =
        if (n <= 1)
          false
        else
          !(2 until n - 1).exists(n % _ == 0)
    }

    //P32 (**) Determine the greatest common divisor of two positive integer numbers.

    def gcd(a: Int, b: Int) = Stream
        .iterate((a,b)) {case (i,k)=>(k, i % k)}
        .dropWhile(_._2!=0)
        .head._1

    //P33 (*) Determine whether two positive integer numbers are coprime.

    implicit class CoPrime(a: Int) {
      def isCoprimeTo(b: Int): Boolean = gcd(a,b)==1
    }


    //P34 (**) Calculate Euler's totient function phi(m).

    implicit class Totient(a:Int) {
      def totient: Int = (1 until a).count(_.isCoprimeTo(a))
    }

    //P35 (**) Determine the prime factors of a given positive integer.
      //??? probably regular recursive solutions is more readable

    implicit class PrimeFactors(n:Int) {

      def primeFactors:List[Int] = pfInfo
        .flatMap(_._2)

      protected def pfInfo = Stream
        .iterate(f(n,primeNumbers))(z=>f(z._1, z._3))
        .takeWhile(k=> !(k._1==1 && k._2.isEmpty) )
        .toList

      private val primeNumbers = Stream.from(1).filter(_.isPrime)

      private def f(number: Int, pNumbs: Seq[Int]) = {
        val steps = legalDivSteps(number, pNumbs.head)
        (steps.last.toInt, Seq.tabulate(steps.size - 1)(_=>pNumbs.head), pNumbs.tail)
      }

      private def legalDivSteps(number: Int, pn: Int) = Stream
        .iterate(number.toFloat)(_ / pn)
        .takeWhile(_.isValidInt)

    }

    //P36 (**) Determine the prime factors of a given positive integer (2).

     implicit class PrimeFactors2 (n: Int) extends PrimeFactors(n)  {

      def primeFactorMultiplicity: List[(Int,Int)] = pfInfo
        .filter(_._2.nonEmpty)
        .map(k=>(k._2.head,k._2.size))

     }

    //P37 (**) Calculate Euler's totient function phi(m) (improved).

    import Math._

    implicit class TotientImporved(a:Int) {
      def totientImproved: Int = a
        .primeFactorMultiplicity
        .foldLeft(1){ (acc,elem) =>
          acc * (elem._1 -1)*pow(elem._1,elem._2-1).toInt
        }
    }


  }

}
