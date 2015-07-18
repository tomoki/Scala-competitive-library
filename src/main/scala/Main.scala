import scala.io.StdIn.readLine
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec

// PriorityQueue[Long]()(scala.math.Ordering.Long.reverse)
package net.pushl {
  package number {
    // Prime  (Prime.scala)
    // Number (Number.scala)
  }
  package string {
    // RollingHash (RollingHash.scala)
  }
  package geometry {
    // Point   (Geometry.scala)
    // Segment (Geometry.scala)
    // Line    (Geometry.scala)
  }
  package datastructure {
    // UnionFindTree (UnionFind.scala)
  }
  object EnRich {
    implicit class AString(val self : String) extends AnyVal {
      def splitToIntVector    = self.split(" ").map(_.toInt).toVector
      def splitToDoubleVector = self.split(" ").map(_.toDouble).toVector

      def splitToDoubleTuple2 = splitTuple2(() => splitToDoubleVector)
      def splitToIntTuple2    = splitTuple2(() => splitToIntVector)
      def splitToDoubleTuple3 = splitTuple3(() => splitToDoubleVector)
      def splitToIntTuple3    = splitTuple3(() => splitToIntVector)

      private def splitTuple2[X](splitTo: () => Vector[X]) = {
        val ab = splitTo()
        (ab(0),ab(1))
      }
      private def splitTuple3[X](splitTo: () => Vector[X]) = {
        val abc = splitTo()
        (abc(0),abc(1),abc(2))
      }
    }
  }
}

import net.pushl.EnRich._
object Main {
  def main(args : Array[String]) : Unit = {
  }
}
