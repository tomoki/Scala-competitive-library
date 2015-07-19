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
  package io {
    import java.io.{BufferedReader, PrintWriter, PrintStream, PushbackReader}
    class MyConsole(val in: BufferedReader, val _out: PrintStream,
                    val _err: PrintStream) {
      // PrintWriter do not flush automatically
      val out = new PrintWriter(_out,false)
      // If argument is null, there are ambiguous which function will be called
      def print(obj: Any)                  = out.print(if(obj == null) "null" else obj.toString)
      def println()                        = out.println
      def println(obj: Any)                = out.println(obj)
      def printf(text: String, args: Any*) = out.printf(text.format(args : _*))
      // NOTE: YOU MUST FLUSH BEFORE END OF MAIN
      def flush()                          = out.flush

      def nextInt()  : Int  = nextLong().toInt
      def nextLong() : Long = {
        if(!goNextValuable())
          throw new NoSuchElementException("Reading long failed")
        else{
          val sgn = if(peek() == '-') -1l else 1
          @tailrec
          def readLong(next: Int, cur: Long) : Long =
            if('0' <= next && next <= '9')
              readLong(readWithoutCheckingPeeked(), cur*10 + next-'0')
            else if(isSpaceOrControl(next))
              sgn*cur
            else
              throw new NumberFormatException(s"readLong found strange byte $next")
          readLong(read(),0)
        }
      }
      def nextString() : String = {
        if(!goNextValuable())
          throw new NoSuchElementException("Reading String failed")
        else {
          val builder = new StringBuilder
          @tailrec
          def appendCode(next: Int) : String = {
            if(isSpaceOrControl(next)){
              builder.toString
            }else{
              builder.append(next.toChar)
              appendCode(read())
            }
          }
          appendCode(read())
        }
      }
      // helpers
      private[this] var peeked: Option[Int] = None
      @inline private def read() =
        peeked match {
          case None    => in.read()
          case Some(a) => { peeked = None; a }
        }
      @inline private def readWithoutCheckingPeeked() = in.read()
      @inline private def peek() =
        peeked match {
          case None    => {
            val res = in.read()
            peeked  = Some(res)
            res
          }
          case Some(a) => a
        }
      @inline private def isThereReadable() = peek() != -1
      // XXX: this limits c is ASCII?
      @inline private def isSpaceOrControl(c: Int) = c <= 32 || c == 127
      @tailrec
      final private def goNextNotSpaceNorControl() : Unit = {
        if(isSpaceOrControl(peek())) {
          read()
          goNextNotSpaceNorControl()
        }else {}
      }
      @inline private def goNextValuable() = {
        goNextNotSpaceNorControl()
        isThereReadable()
      }
    }
  }
}

import net.pushl.io.MyConsole
object Main {
  def main(args: Array[String]) : Unit = {
    val console = new MyConsole(Console.in, Console.out, Console.err)
    val solver  = new Solver(console)
    solver.main()
    console.flush()
  }
}


class Solver(val stdio: MyConsole){
  import stdio._ // shadow Console.~
  def main() : Unit = {
    // write here.
  }
}

