package net.pushl.number.mod {
  class Mod(val v: Long) extends AnyVal {
    override def toString() = s"Mod($v)"
    def pow(n: Long)(implicit frac: ModFractional) : Mod =
      n match {
        case 0 => Mod(1)
        case 1 => this
        case _ if n % 2 == 0 => (frac.times(this,this)).pow(n/2)
        case _               => frac.times(this, (frac.times(this,this)).pow(n/2))
      }
  }
  object Mod {
    def apply(i: Int)(implicit frac : ModFractional) = frac.fromInt(i)
  }

  // TODO: Implement div
  class ModFractional(val mod: Long) extends Numeric[Mod]{
    def compare(x: Mod, y: Mod) = (x.v%mod).compare(y.v%mod)
    def fromInt(x: Int)         = new Mod(x.toLong % mod)
    def minus(x: Mod, y: Mod)   = new Mod((x.v + mod) - y.v % mod)
    def negate(x: Mod)          = new Mod((mod - x.v) % mod)
    def plus(x: Mod, y: Mod)    = new Mod((x.v + y.v) % mod)
    def times(x: Mod, y: Mod)   = new Mod(x.v * y.v % mod)
    def toDouble(x: Mod) = x.v.toDouble
    def toFloat(x: Mod)  = x.v.toFloat
    def toInt(x: Mod)    = x.v.toInt
    def toLong(x: Mod)   = x.v.toLong
  }
}
// import net.pushl.number.mod._
// implicit val modfrac = new ModFractional(m)
// import scala.math.Fractional.Implicits._
// import scala.math.Numeric.Implicits._
