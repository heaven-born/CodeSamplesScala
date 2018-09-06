object CodeSamplesArithmetic {

  def main(args: Array[String]): Unit = {


    //P31 (**) Determine whether a given integer number is prime.

    implicit class Prime(n:Int) {
      def isPrime: Boolean = if (n<=1) false else !(2 until n - 1).toStream.exists(n % _ == 0)
    }


    //P32 (**) Determine the greatest common divisor of two positive integer numbers.

    def gcd(a: Int, b: Int) = {
      lazy val stream : Stream[(Int,Int)] =  (a,b) #:: stream.map{case (i,k)=>(k, i % k)}
      stream.dropWhile(_._2!=0).head._1
    }


  }

}
