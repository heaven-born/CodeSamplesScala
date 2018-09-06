object CodeSamplesArithmetic {

  def main(args: Array[String]): Unit = {


    //P31 (**) Determine whether a given integer number is prime.

    implicit class Prime(n:Int) {
      def isPrime: Boolean = if (n<=1) false else !(2 until n - 1).toStream.exists(n % _ == 0)
    }

  }

}
