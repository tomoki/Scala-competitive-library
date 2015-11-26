package net.pushl.number {
  import scala.math.BigInt
  class Fraction(n: BigInt, d: BigInt){
    assert(d != 0)
    private val g = n gcd d
    val numerator   = n * d.signum / g
    val denominator = d * d.signum / g
    override def toString(): String = s"Fraction(${numerator},${denominator})"


    // for inheritance.
    def canEqual(other: Any) = other.isInstanceOf[Fraction]
    override def equals(other: Any) = {
      other match {
        // TODO: Refactoring.
        case that: Fraction =>
          that.canEqual(Fraction.this) &&
            ((numerator * that.denominator).compare(denominator * that.numerator) == 0)
        case _              => false
      }
    }
  }
  object Fraction {
    def apply(n: BigInt) = new Fraction(n,1)
    def apply(n: BigInt, d: BigInt) = new Fraction(n,d)
  }
  object FractionImplicits {
    implicit object FractionIsFractional extends Fractional[Fraction] {
      def compare(x: Fraction, y: Fraction) : Int = (x.numerator * y.denominator) compare (x.denominator * y.numerator)
      def plus(x: Fraction,  y: Fraction) : Fraction = Fraction(x.numerator * y.denominator + x.denominator * y.numerator,
                                                                x.denominator * y.denominator)
      def minus(x: Fraction, y: Fraction) : Fraction = plus(x,negate(y))
      def times(x: Fraction, y: Fraction) : Fraction = Fraction(x.numerator * y.numerator, x.denominator * y.denominator)
      def div(x: Fraction,   y: Fraction) : Fraction = times(x, Fraction(y.denominator, y.numerator))
      def negate(x: Fraction) : Fraction  = Fraction(-x.numerator, x.denominator)
      def fromInt(x: Int)     : Fraction  = Fraction(x,1)
      def toInt(x: Fraction)     : Int    = (x.numerator / x.denominator).toInt
      def toLong(x: Fraction)    : Long   = (x.numerator / x.denominator).toLong
      def toFloat(x: Fraction)   : Float  = toDouble(x).toFloat
      def toDouble(x : Fraction) : Double = x.numerator.toDouble / x.denominator.toDouble
    }
  }
}
// Copy following.
// import scala.math.Fractional.Implicits._
// import net.pushl.number.FractionImplicits._
// import scala.math.Ordering.Implicits._
// import net.pushl.number.Fraction
