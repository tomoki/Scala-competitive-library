import scala.annotation.tailrec
import net.pushl.io.MyConsole

package net.pushl {
  package number {
    // Prime    (Prime.scala)
    // Number   (Number.scala)
    // Combi    (Combi.scala)
    // Optimize (Optimize.scala)
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
  }
}









// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
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
        if('0' <= next && next <= '9'){
          val _ = read() // is equal to next
          readLong(peek, cur*10 + next-'0')
        }
        else if(isEnd(next) || isSpaceOrControl(next))
          sgn*cur
        else
          throw new NumberFormatException(s"readLong found strange byte $next")
      val res = readLong(peek,0)
      skipTrailingSpaces()
      res
    }
    def nextString() : String = {
      if(!goNextValuable())
        throw new NoSuchElementException("Reading String failed")
      val builder = new StringBuilder
      @tailrec
      def appendCode(next: Int) : String = {
        if(isEnd(next) || isSpaceOrControl(next)){
          builder.toString
        }else{
          builder.append(read().toChar) // here we skip.
          appendCode(peek)
        }
      }
      val res = appendCode(peek)
      skipTrailingSpaces()
      res
    }
    // TODO: refactoring to marge nextString
    def readLine() : String = {
      if(isEnd(peek))
        throw new NoSuchElementException("Reading Line failed")
      skipNewline()
      val builder = new StringBuilder
      @tailrec
      def appendCode(next: Int) : String = {
        if(isEnd(next) || isNewLine(next)){
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
    private[this] var is_first = true
    private def read() = {
      val res = peeked match {
        case None    => in.read()
        case Some(a) => { peeked = None; a }
      }
      is_first = false
      res
    }
    private def peek() =
      peeked match {
        case None    => {
          val res = in.read()
          peeked  = Some(res)
          res
        }
        case Some(a) => a
      }
    private def isEnd(c: Int)     = c == -1
    private def isCR(c: Int)      = c == 13
    private def isLF(c: Int)      = c == 10
    private def isNewLine(c: Int) = isLF(c) || isCR(c)
    private def isThereReadable() = !isEnd(peek)
    private def isSpaceOrControl(c: Int) = (0 <= c && c <= 32) || c == 127
    @tailrec
    final private def goNextNotSpaceNorControl() : Unit =
      if(isSpaceOrControl(peek)){
        read()
        goNextNotSpaceNorControl()
      }
    final private def skipTrailingSpaces() : Unit = {
      @tailrec
      def skipTrailingSpacesAux() : Unit = {
        if(!isNewLine(peek) && isSpaceOrControl(peek)){
          read()
          skipTrailingSpacesAux()
        }
      }
      skipTrailingSpacesAux()
    }
    final private def skipNewline() : Unit = {
      if(is_first){
      }else if(isCR(peek)){
        read()
        if(isLF(peek)) read()
      }else if(isLF(peek)){
        read()
      }
    }
    private def goNextValuable() = {
      goNextNotSpaceNorControl()
      isThereReadable()
    }
    private def parseLineToVector[X](parser: () => X) : Vector[X] = {
      import scala.collection.immutable.VectorBuilder
      val vb = new VectorBuilder[X]()
      skipNewline()
      @tailrec
      def parseLineToVectorAux() : Vector[X] =
        if(isNewLine(peek) || isEnd(peek)) {
          vb.result
        }else{
          vb += parser()
          parseLineToVectorAux()
        }
      parseLineToVectorAux()
    }
  }
}

