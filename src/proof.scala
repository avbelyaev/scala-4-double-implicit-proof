abstract class customNumeric[T] {
	def div(x: T, y: T): T
	def mul(x: T, y: T): T
	def sub(x: T, y: T): T
}

object customNumeric {

	implicit object FloatIsMyNumeric extends customNumeric[Float] {
		def div(x: Float, y: Float): Float = x / y
		def mul(x: Float, y: Float): Float = x * y
		def sub(x: Float, y: Float): Float = x - y
	}

	implicit object DoubleIsMyNumeric extends customNumeric[Double] {
		def div(x: Double, y: Double): Double = x / y
		def mul(x: Double, y: Double): Double = x * y
		def sub(x: Double, y: Double): Double = x - y
	}

	implicit object IntIsMyNumeric extends customNumeric[Int] {
		def div(x: Int, y: Int): Int = x / y
		def mul(x: Int, y: Int): Int = x * y
		def sub(x: Int, y: Int): Int = x - y
	}

	implicit object LongIsMyNumeric extends customNumeric[Long] {
		def div(x: Long, y: Long): Long = x / y
		def mul(x: Long, y: Long): Long = x * y
		def sub(x: Long, y: Long): Long = x - y
	}

	implicit object FractionIntIsCustomNumeric extends customNumeric[Fraction[Int]] {
		def div(x: Fraction[Int], y: Fraction[Int]): Fraction[Int] =
			new Fraction[Int](x.numerator*y.denominator, x.denominator*y.numerator)

		def mul(x: Fraction[Int], y: Fraction[Int]): Fraction[Int] =
			new Fraction[Int](x.numerator*y.numerator, x.denominator*y.denominator)

		def sub(x: Fraction[Int], y: Fraction[Int]): Fraction[Int] =
			new Fraction[Int](x.numerator*y.denominator - y.numerator*x.denominator, x.denominator*y.denominator)
	}

	implicit object FractionLongIsCustomNumeric extends customNumeric[Fraction[Long]] {
		def div(x: Fraction[Long], y: Fraction[Long]): Fraction[Long] =
			new Fraction[Long](x.numerator*y.denominator, x.denominator*y.numerator)

		def mul(x: Fraction[Long], y: Fraction[Long]): Fraction[Long] =
			new Fraction[Long](x.numerator*y.numerator, x.denominator*y.denominator)

		def sub(x: Fraction[Long], y: Fraction[Long]): Fraction[Long] =
			new Fraction[Long](x.numerator*y.denominator - y.numerator*x.denominator, x.denominator*y.denominator)
	}

	//implicit def intToFraction(x: Int): Fraction[Int] = new Fraction[Int](x, 1)
	//implicit def intToFraction(x: Long): Fraction[Long] = new Fraction[Long](x, 1)
	//implicit def int2ToFraction(x: Int, y: Int): Fraction[Int] = new Fraction[Int](x, y)
}




class Fraction[T] (n: T, d: T)(implicit num: customNumeric[T]) {
	val numerator : T = num.div(n, gcd(n, d))
	val denominator : T = num.div(d, gcd(n, d))
	//нужна арифметика целых типов
	def gcd (x: T, y: T): T = if (0 == y) x else gcd(y, num.sub(x, num.mul(y, num.div(x, y))))

	override def toString = {
		if (0 == numerator) "{0}"
		else if (1 == denominator) "{" + numerator + "}"
		else if (numerator == denominator) "{1}"
		else "{" + numerator + "}/{" + denominator + "}"
	}
}

object Fraction {
	implicit def int2Fraction(x: Int): Fraction[Int] = new Fraction[Int](x, 1)
	implicit def long2Fraction(x: Long): Fraction[Long] = new Fraction[Long](x, 1)
}




class EquationSystem[T] (val a: T, val b: T, val c: T,
								 val d: T, val e: T, val f: T) {

	def solve(implicit num : customNumeric[T]): Option[(T, T)] = {
		Option(
			num.div(
				num.sub(num.mul(c, e), num.mul(b, f)),
				num.sub(num.mul(a, e), num.mul(b, d))),

			num.div(
				num.sub(num.mul(f, a), num.mul(c, d)),
				num.sub(num.mul(a, e), num.mul(b, d)))
		)
	}

	override def toString = "\n" +
	  "(" + a + ")*X + (" + b + ")*Y = " + c + "\n" +
	  "(" + d + ")*X + (" + e + ")*Y = " + f
}

object Main {
	def main(args: Array[String]): Unit = {

		val esf = new EquationSystem[Float](2,-1,7,2,3,1)
		println(esf)
		println("float: " + esf.solve)

		val esd = new EquationSystem[Double](2,-1,7,2,3,1)
		println("double: " + esd.solve)

		val esi = new EquationSystem[Fraction[Int]](2,-1,7,2,3,1)
		println("intFrac: " + esi.solve)

		val esl = new EquationSystem[Fraction[Long]](2l,-1l,7l,2l,3l,1l)
		println("longFrac: " + esl.solve)
	}
}