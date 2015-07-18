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
    // Point
    // Segment
    // Line
  }
  object EnRich {
    implicit class AString(val self : String) extends AnyVal {
      def splitToIntArray = self.split(" ").map(_.toInt)
    }
  }
}

object Main {
  def main(args : Array[String]) : Unit = {
  }
}
