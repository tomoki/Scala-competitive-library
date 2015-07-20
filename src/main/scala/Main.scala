import scala.annotation.tailrec
import net.pushl.io.MyConsole

object Main {
  def main(args: Array[String]) : Unit = {
    val console = new MyConsole(Console.in, Console.out, Console.err)
    val solver  = new Solver(console)
    solver.main()
    console.flush()
  }
}
package net.pushl.io {
  import java.io.{BufferedReader, PrintWriter, PrintStream, PushbackReader}
  class MyConsole(val in: BufferedReader, val _out: PrintStream,
                  val err: PrintStream) {
    // PrintWriter do not flush automatically
    val out = new PrintWriter(_out,false)
    // If argument is null, there are ambiguous which function will be called
    def print(obj: Any)                  = out.print(if(obj == null) "null" else obj.toString)
    def println()                        = out.println()
    def println(obj: Any)                = out.println(obj)
    def printf(text: String, args: Any*) = out.printf(text.format(args : _*))
    // NOTE: YOU MUST FLUSH BEFORE END OF MAIN
    def flush()                          = out.flush()
    def debugln(obj: Any)                = err.println(obj)

    def readIntVector()    : Vector[Int]    = parseLineToVector(() => nextInt)
    def readLongVector()   : Vector[Long]   = parseLineToVector(() => nextLong)
    def readStringVector() : Vector[String] = parseLineToVector(() => nextString)
    def nextInt()          : Int            = nextLong().toInt
    def nextBigInt()       : BigInt         = BigInt(nextString())
    def nextBigDecimal(  ) : BigDecimal     = BigDecimal(nextString())
    def nextDouble()       : Double         = nextString().toDouble
    def nextLong() : Long = {
      if(!goNextValuable())
        throw new NoSuchElementException("Reading long failed")
      val sgn = if(peek == '-') -1l else 1
      if(sgn == -1l) read()
      if(peek < '0' || '9' < peek)
        throw new NumberFormatException(s"readLong found only '-' or no number")
      @tailrec
      def readLong(next: Int, cur: Long) : Long =
        if('0' <= next && next <= '9')
          readLong(readWithoutCheckingPeeked(), cur*10 + next-'0')
        else if(endInt(next) || isSpaceOrControl(next))
          sgn*cur
        else
          throw new NumberFormatException(s"readLong found strange byte $next")
      val res = readLong(read(),0)
      skipTrailingSpaces()
      res
    }
    def nextString() : String = {
      if(!goNextValuable())
        throw new NoSuchElementException("Reading String failed")
      val builder = new StringBuilder
      @tailrec
      def appendCode(next: Int) : String = {
        if(endInt(next) || isSpaceOrControl(next)){
          builder.toString
        }else{
          builder.append(next.toChar)
          appendCode(readWithoutCheckingPeeked())
        }
      }
      val res = appendCode(read())
      skipTrailingSpaces()
      res
    }
    // TODO: refactoring to marge nextString
    def readLine() : String = {
      if(endInt(peek))
        throw new NoSuchElementException("Reading Line failed")
      val builder = new StringBuilder
      @tailrec
      def appendCode(next: Int) : String = {
        if(endInt(next) || isNewLine(next)){
          builder.toString
        }else{
          builder.append(next.toChar)
          appendCode(read())
        }
      }
      appendCode(read())
    }
    // helpers
    private[this] var peeked: Option[Int] = None
    private[this] var last = -1
    private def read() = {
      val res = peeked match {
          case None    => in.read()
          case Some(a) => { peeked = None; a }
        }
      last = res
      res
    }
    @inline private def readWithoutCheckingPeeked() = {
      val res = in.read()
      last = res
      res
    }
    @inline private def peek() =
      peeked match {
        case None    => {
          val res = in.read()
          peeked  = Some(res)
          res
        }
        case Some(a) => a
      }
    @inline private def endInt(c: Int)    = c == -1
    @inline private def isNewLine(c: Int) = c == 10 || c == 13     // LF and CR
    @inline private def isThereReadable() = !endInt(peek)
    // XXX: this limits c is ASCII?
    @inline private def isSpaceOrControl(c: Int) = (0 <= c && c <= 32) || c == 127
    @tailrec
    final private def goNextNotSpaceNorControl() : Unit =
      if(isSpaceOrControl(peek)){
        read()
        goNextNotSpaceNorControl()
      }
    final private def skipTrailingSpaces() : Unit = {
      @tailrec
      def skipTrailingSpacesAux() : Unit = {
        if(!isNewLine(last) && !isNewLine(peek) && isSpaceOrControl(peek)){
          read()
          skipTrailingSpacesAux()
        }
      }
      skipTrailingSpacesAux
      if(!isNewLine(last) && isNewLine(peek)) read()
    }
    @tailrec
    final private def skipTrailingSpacesAndNewline() : Unit =
      if(isNewLine(peek)){
        val _ = read() // windows causes error. maybe.
      }else if(isSpaceOrControl(peek)){
        read()
        skipTrailingSpacesAndNewline()
      }
    @inline private def goNextValuable() = {
      goNextNotSpaceNorControl()
      isThereReadable()
    }
    @inline private def parseLineToVector[X](parser: () => X) : Vector[X] = {
      import scala.collection.immutable.VectorBuilder
      val vb = new VectorBuilder[X]()
      @tailrec
      def parseLineToVectorAux(first: Boolean=false) : Vector[X] =
        if((!first && isNewLine(last)) || isNewLine(peek) || endInt(peek)) {
          vb.result
        }else{
          vb += parser()
          parseLineToVectorAux()
        }
      parseLineToVectorAux(true)
    }
  }
}

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
  }
}

// Document: http://www.scala-lang.org/api/current/#package
// -----------------------------------------------------------------------------
class Solver(val stdio: MyConsole){
  import stdio._ // shadow Console.~
  def main() : Unit = {
    // write here.
  }
}

