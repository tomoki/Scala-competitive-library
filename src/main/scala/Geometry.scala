package net.pushl {
  package geometry {
    // can be used as vector from zero point
    class Point(val y: Double, val x: Double) extends Ordered[Point] {
      // normally, Euclidean norm is abs, but it has high cost.
      val EPS = 1e-7 // TODO: make it private final
      def norm() = x*x + y*y
      def abs()  = Math.sqrt(this.norm)
      def distanceTo(that: Point) = (that-this).lengthAsVector
      def lengthAsVector() = abs
      // rotate by theta (-pi,+pi)
      //  TODO: validate rotate
      def rotate(theta: Double) = {
        val cos_theta = Math.cos(theta)
        val sin_theta = Math.sin(theta)
        Point(x*cos_theta-y*sin_theta, x*sin_theta+y*cos_theta)
      }
      // TODO: validate these.
      // 内積: a・b = |a||b| cosθ
      //            = ax*bx + ay*by
      def dot(that: Point) =
        this.x*that.x + this.y+that.y
      // 外積(の大きさ) |a x b| = |a||b|sinθ
      //   外積はa から bへの右ねじの進む向きへのベクトル(三次元)
      def cross(that: Point) =
        this.x*that.y - this.y*that.x

      // FIXME: it is square area.
      def compare(that: Point) : Int = {
        val xdiff = Math.abs(this.x-that.x)
        val ydiff = Math.abs(this.y-that.y)
        if(xdiff <= EPS && ydiff <= EPS)          0
        else if(xdiff <= EPS && this.y < that.y) -1
        else if(xdiff <= EPS && this.y > that.y) +1
        else if(this.x < that.x)                 -1
        else                                     +1
      }
      def +(that: Point) = Point(this.y+that.y,this.x+that.x)
      def -(that: Point) = this + (-that)
      def unary_+ = this
      def unary_- = Point(-this.y,-this.x)
      def /(that: Double)= this*(1/that)
      def *(that: Double)= Point(this.y*that,this.x*that)
      def canEqual(that: Any) = that.isInstanceOf[Point]
      override def equals(that: Any) = that match {
          case that: Point => that.canEqual(this) && (this.compare(that) == 0)
          case _           => false
        }
      override def toString() = s"($y, $x)"
      // TODO: validate hashcode.
      override def hashCode() = {
        val prime = 41
        val ax = Math.round(x+EPS) min Math.round(x-EPS)
        val ay = Math.round(y+EPS) min Math.round(y-EPS)
        prime * ax.hashCode + ay.hashCode
      }
    }
    object Point {
      def zero() = this(0,0)
      def apply(y: Double, x: Double) = new Point(y, x)
    }

    class Segment(val a: Point, val b: Point) {
      val EPS = 1e-7 // TODO: make it private final
      // TODO: test this
      // def isIntersected(that: Segment) = {
      //   val this_a_x  = a.x
      //   val this_b_x = b.x
      //   val this_upper_y = (a.y) max (b.y)
      //   val this_lower_y = (a.y) min (b.y)
      //   val that_a_x  = that.a.x
      //   val that_b_x = that.b.x
      //   val that_upper_y = (that.a.y) max (that.b.y)
      //   val that_lower_y = (that.a.y) min (that.b.y)

      //   val no_chance = (this_b_x + EPS < that_a_x)  ||
      //                   (that_b_x + EPS < this_a_x)  ||
      //                   (that_upper_y + EPS < this_lower_y) ||
      //                   (this_upper_y + EPS < that_lower_y)

      //   !no_chance &&
      //   (((b-a) cross (that.a-a)) *
      //    ((b-a) cross (that.b-a)) < EPS) &&
      //   (((that.b-that.a) cross (a -that.a)) *
      //    ((that.b-that.a) cross (b-that.a)) < EPS)
      // }
      override def toString() = s"S: $a -- $b"
    }
    object Segment {
      def apply(p1: Point, p2: Point) =  new Segment(p1,p2)
    }
    class Line(val a: Point, val b: Point) {
      val EPS = 1e-7
      def isParallelTo(that: Line)       = Math.abs((a-b) cross (that.a - that.b)) < EPS
      def isIntersectedTo(that: Line)    = !isParallelTo(that)
      def isIntersectedTo(that: Segment) = onThisLine(that.a) || onThisLine(that.b) ||
                                           (toClockWise(that.a) != toClockWise(that.b))

      def distanceTo(that: Point)  = Math.abs(((a-that) cross (b-that)) / (a-b).abs)
      def onThisLine(that: Point)  = distanceTo(that) < EPS
      def toClockWise(that: Point) = if(onThisLine(that))
                                          sys.error("Before clockwise, check on this line")
                                        else
                                          ((b-that) cross (a-that)) > 0

      override def toString() = s"L:-- $a -- $b --"
    }
    object Line {
      def apply(p1: Point, p2: Point) = if(p1 == p2)
                                          sys.error("Start and end of Line should be different")
                                        else
                                          new Line(p1,p2)
    }
  }
}
