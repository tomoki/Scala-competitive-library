
package net.pushl.number.matrix {
  // TODO: Use Numeric
  class Matrix[T](private val data: Vector[Vector[T]])(implicit num: Numeric[T]) {
    require(data.length != 0, "height should not be zero")
    require(data.map(_.length).distinct.size == 1, "every width should be equal")
    require(data(0).length != 0, "width should not be zero")
    def height = data.length
    def width  = data(0).length
    def apply(i: Int) : Vector[T] = data(i)
    def +(that: Matrix[T]) : Matrix[T] = {
      require(this.height == that.height, "plus requirement")
      require(this.width == that.width, "plus requirement")
      new Matrix((this.data zip that.data).map(
                   {case (l1: Vector[T], l2: Vector[T]) =>
                     (l1 zip l2).map(
                       {case (l, r) => num.plus(l, r)})}))
    }
    def *(that: Matrix[T]) : Matrix[T] = {
      require(this.width  == that.height, "multiply requirement")
      val line = this.width
      def gen(y: Int, x: Int) : T = {
        (0 until line).map(i => num.times(this.data(y)(i), that.data(i)(x))).sum
      }
      Matrix.generate(this.height, that.width, gen)
    }
    def -(that: Matrix[T]) : Matrix[T] =
      this + that.map(i => num.negate(i))
    def pow(n: Long) : Matrix[T] = {
      n match {
        case 0 => Matrix.identity(this.width)
        case 1 => this
        case _ if n % 2 == 0 => (this * this).pow(n/2)
        case _               => (this * this).pow(n/2) * this
      }
    }
    def map[A](f: T => A)(implicit num: Numeric[A]) : Matrix[A] = {
      new Matrix(this.data.map(_.map(f(_))))
    }
    override def toString() : String = s"Matrix($data)"
  }
  object Matrix {
    def identity[T](n: Int)(implicit num: Numeric[T]): Matrix[T] =
      generate(n, n, (y, x) => if(y == x) num.one else num.zero)

    def generate[T](h: Int, w: Int, g: (Int, Int) => T)(implicit num: Numeric[T]) : Matrix[T] = {
      val v = for(i <- 0 until h;
                  j <- 0 until w) yield g(i, j)
      new Matrix(v.grouped(w).map(_.toVector).toVector)
    }

    def apply[T](data: Vector[Vector[T]])(implicit num: Numeric[T]) : Matrix[T] =
      new Matrix(data)
  }
}
// import net.pushl.number.matrix._
