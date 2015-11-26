import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class TestFraction extends FlatSpec with Checkers {
  import scala.math.Fractional.Implicits._
  import net.pushl.number.FractionImplicits._
  import scala.math.Ordering.Implicits._
  import net.pushl.number.Fraction

  "Fraction's denominator and numerator " should "be disjoint" in {
    check((a: Int, b: Int) => {
            if(b != 0) {
              val c = Fraction(a,b)
              a == 0 || (c.denominator gcd c.numerator) == BigInt(1)
            }else
              true
          })
  }
  "Fraction's denominator " should "be positive" in {
    check((a: Int, b: Int) => {
            if(b != 0) {
              val c = Fraction(a,b)
              c.denominator >= 0
            }else
              true
          })
  }

  "Fraction" should "have orders" in {
    check((a: Int, b: Int, c: Int, d: Int) => {
            if(b != 0 && c != 0 && d != 0){
              val k1 = Fraction(a,b)
              val k2 = Fraction(c,d)
              k1 < k2 || k1 > k2 || k1 == k2
            }else{
              true
            }
          })
  }
  "Fraction(0,n)"  should "be equal to Fraction(0,1)" in {
    check((a: Int) => {
            if(a != 0){
              Fraction(0,a) == Fraction(0,1)
            }else{
              true
            }
          })
  }
  "Fraction's div" should "be equal to 1/times" in {
    check((a: Int, b: Int, c: Int, d: Int) => {
            if(b != 0 && c != 0 && d != 0){
              val k1 = Fraction(a,b)
              val k2 = Fraction(c,d)
              k1 * k2 == k1 / (Fraction(1,1) / k2)
            }else{
              true
            }
          })
  }
  "Fraction's double negated" should "be equal to itself" in {
    check((a: Int, b: Int) => {
            if(b != 0){
              Fraction(a,b) == -(-(Fraction(a,b)))
            }else
              true
          })
  }
  "a + b - b " should "a" in {
    check((a: Int, b: Int, c: Int, d: Int) => {
            if(b != 0 && c != 0 && d != 0){
              val k1 = Fraction(a,b)
              val k2 = Fraction(c,d)
              k1 + k2 - k2 == k1
            }else{
              true
            }
          })
  }
  "a - b " should "be a + (-b)" in {
    check((a: Int, b: Int, c: Int, d: Int) => {
            if(b != 0 && c != 0 && d != 0){
              val k1 = Fraction(a,b)
              val k2 = Fraction(c,d)
              k1 - k2 == k1 + (-k2)
            }else{
              true
            }
          })
  }
}

