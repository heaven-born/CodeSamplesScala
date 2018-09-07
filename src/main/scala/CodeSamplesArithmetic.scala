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
      //??? probably regular recursive solutions it more readable

    implicit class PrimeFactors(n:Int) {

      def primeFactors:List[Int] = Stream
        .iterate(f(n,primeNumbers))(z=>f(z._1, z._3))
        .takeWhile(k=> !(k._1==1 && k._2.isEmpty) )
        .toList
        .flatMap(_._2)

      private val primeNumbers = Stream.from(1).filter(_.isPrime)

      private def f(number: Int, pNumbs: Seq[Int]) = {
        val steps = legalDivSteps(number, pNumbs.head)
        (steps.last.toInt, Seq.tabulate(steps.size - 1)(_=>pNumbs.head), pNumbs.tail)
      }

      private def legalDivSteps(number: Int, pn: Int) = Stream
        .iterate(number.toFloat)(_ / pn)
        .takeWhile(_.isValidInt)

    }
    //println(315.primeFactors)




  }

}
