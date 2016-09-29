abstract class customNumeric[T] {
	def div(x: T, y: T): T
}

object customNumeric {

	implicit object FloatIsMyNumeric extends customNumeric[Float] {
		def div(x: Float, y: Float): Float = x / y
	}

	/*implicit object IntIsCustomNumeric extends customNumeric[Int] {
		def div(x: Int, y: Int): Int = x / y
	}*/

	//не проимходит преобразование!?
	implicit object FractionIsCustomNumeric extends customNumeric[Fraction[Int]] {
		def div(x: Fraction[Int], y: Fraction[Int]): Fraction[Int] = new Fraction[Int](x.numerator*y.denominator, x.denominator*y.numerator)
	}

	implicit def intToFraction(x: Int): Fraction[Int] = new Fraction[Int](x, 1)
}

class Fraction[T] (n: T, d: T)/*(implicit num: customNumeric[T])*/ {
	//нужна арифметика для целочисленных типов
	val numerator : T = n//num.div(n, gcd(n, d))
	val denominator : T = d//num.div(d, gcd(n, d))

	//def gcd (x: T, y: T): T = if (0 == y) x else gcd(y, num.sub(x, num.mul(y, num.div(x, y))))

	override def toString = {
		if (0 == numerator) "{0}"
		else if (1 == denominator) "{" + numerator + "}"
		else if (numerator == denominator) "{1}"
		else "{" + numerator + "}/{" + denominator + "}"
	}
}

object Fraction {
	implicit def int2Fraction(x: Int): Fraction[Int] = new Fraction[Int](x, 1)
}


class EquationSystem[T] (val a: T, val b: T, val c: T,
								 val d: T, val e: T, val f: T) {

	override def toString = "\n" +
	  a + "*X + " + b + "*Y = " + c + "\n" +
	  d + "*X + " + e + "*Y = " + f

	def solve(implicit num : customNumeric[T]): Option[(T, T)] = {
		Option(num.div(a,b), num.div(c,d))
	}
}

object Main {
	def main(args: Array[String]): Unit = {
		val si = new EquationSystem[Int](2,-1,7,2,3,1)
		println(si.solve)
	}
}