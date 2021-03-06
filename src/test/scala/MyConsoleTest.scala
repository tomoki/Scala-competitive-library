import org.scalatest.FlatSpec
import org.scalatest.exceptions.TestFailedException
import org.scalatest.PrivateMethodTester
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Span

class MyConsoleSpec extends FlatSpec with PrivateMethodTester with TimeLimitedTests {
  import org.scalatest.time.SpanSugar._
  val timeLimit = 200 millis
  import net.pushl.io.MyConsole
  // read test
  def generateConsole(input: String) : MyConsole = {
    val ostream = new java.io.ByteArrayOutputStream
    val estream = new java.io.ByteArrayOutputStream

    val i = new java.io.BufferedReader(new java.io.StringReader(input))
    val o = new java.io.PrintStream(ostream)
    val e = new java.io.PrintStream(estream)
    new MyConsole(i,o,e)
  }

  val peek = PrivateMethod[Int]('peek)
  val isThereReadable     = PrivateMethod[Boolean]('isThereReadable)
  val goNextNotSpaceNorControl = PrivateMethod[Unit]('goNextNotSpaceNorControl)
  val goNextValuable = PrivateMethod[Boolean]('goNextValuable)
  "'' -> peek" should "-1" in {
    val c = generateConsole("")
    assert((c invokePrivate peek()) == -1)
  }
  "abc' -> peek" should "a" in {
    val c = generateConsole("abc")
    val p = c invokePrivate peek()
    assert(p == 'a')
  }
  "'' -> isThereReadable" should "false" in {
    val c = generateConsole("")
    assert(!(c invokePrivate isThereReadable()))
  }
  "'abc' -> isThereReadable" should "true" in {
    val c = generateConsole("abc")
    assert((c invokePrivate isThereReadable()))
  }
  "'a' -> goNextNotSpaceNorControl -> peek" should "a" in {
    val c = generateConsole("a")
    c invokePrivate goNextNotSpaceNorControl()
    val p = c invokePrivate peek()
    assert(p == 'a')
  }
  "'' -> goNextNotSpaceNorControl -> peek" should "-1" in {
    val c = generateConsole("")
    c invokePrivate goNextNotSpaceNorControl()
    val p = c invokePrivate peek()
    assert(p == -1)
  }
  "'' -> goNextValuable" should "false" in {
    val c = generateConsole("")
    val b = c invokePrivate goNextValuable()
    assert(!b)
  }
  "'1' -> goNextValuable" should "true" in {
    val c = generateConsole("1")
    val b = c invokePrivate goNextValuable()
    assert(b)
  }
  "'b' -> goNextValuable" should "true" in {
    val c = generateConsole("b")
    val b = c invokePrivate goNextValuable()
    assert(b)
  }
  "'123' -> nextLong one times" should "generate 123" in {
    val c = generateConsole("123")
    val a = c.nextLong
    assert(a == 123l)
  }
  "'-123' -> nextLong two times" should "generate -123" in {
    val c = generateConsole("-123")
    val a = c.nextLong
    assert(a == -123l)
  }
  "'123 456' -> nextLong two times" should "generate 123 456" in {
    val c = generateConsole("123 456")
    val a = c.nextLong
    val b = c.nextLong

    assert(a == 123l)
    assert(b == 456l)
  }
  "'123    456' -> nextLong two times" should "generate 123 456" in {
    val c = generateConsole("123   456")
    val a = c.nextLong
    val b = c.nextLong

    assert(a == 123l)
    assert(b == 456l)
  }
  "'123\n456' -> nextLong two times" should "generate 123 456" in {
    val c = generateConsole("123\n456")
    val a = c.nextLong
    val b = c.nextLong
    assert(a == 123l)
    assert(b == 456l)
  }
  "'' -> nextLong one times" should "cause NoSuchElementException" in {
    val c = generateConsole("")
    var expected_exception_found = false
    try {
      val _ = c.nextLong
      fail("no exception found")
    }catch{
      case e: NoSuchElementException => {expected_exception_found = true}
      case e: TestFailedException    => fail(e.getMessage)
      case _: Throwable              => fail("different exception found")
    }
    assert(expected_exception_found)
  }
  "'-' -> nextLong one times" should "cause NumberFormatException" in {
    val c = generateConsole("-")
    var expected_exception_found = false
    try {
      val _ = c.nextLong
      fail("no exception found")
    }catch{
      case e: NumberFormatException => {expected_exception_found = true}
      case e: TestFailedException   => fail(e.getMessage)
      case e: Throwable             => fail("different exception found")
    }
    assert(expected_exception_found)
  }

  "'-123 -456' -> nextLong two times" should "generate -123 -456" in {
    val c = generateConsole("-123 -456")
    val a = c.nextLong
    val b = c.nextLong
    assert(a == -123l)
    assert(b == -456l)
  }
  "'10000000000000' -> nextLong two times" should "generate 10000000000000" in {
    val c = generateConsole("10000000000000")
    val a = c.nextLong
    assert(a == 10000000000000l)
  }
  "'s' -> nextString" should "generate 's'" in {
    val c = generateConsole("s")
    val a = c.nextString
    assert(a == "s")
  }
  "'str1 str2' -> nextString two times" should "generate 'str1' 'str2'" in {
    val c = generateConsole("str1 str2")
    val a = c.nextString
    val b = c.nextString
    assert(a == "str1")
    assert(b == "str2")
  }
  "'' -> nextString" should "cause NoSuchElementException" in {
    val c = generateConsole("")
    var expected_exception_found = false
    try {
      val _ = c.nextString
      fail("no exception found")
    }catch{
      case e: NoSuchElementException => {expected_exception_found = true}
      case e: TestFailedException    => fail(e.getMessage)
      case _: Throwable              => fail("different exception found")
    }
    assert(expected_exception_found)
  }
  ("'10000000000000000000000000000000000000' -> nextLong two times"
     should "generate 10000000000000000000000000000000000000") in {
    val c = generateConsole("10000000000000000000000000000000000000")
    val a = c.nextBigInt
    assert(a == BigInt("10000000000000000000000000000000000000"))
  }
  ("'-10000000000000000000000000000000000000' -> nextLong two times"
     should "generate -10000000000000000000000000000000000000") in {
    val c = generateConsole("-10000000000000000000000000000000000000")
    val a = c.nextBigInt
    assert(a == BigInt("-10000000000000000000000000000000000000"))
  }

  "'-1.23' -> nextDouble" should "generate -1.23" in {
    val c = generateConsole("-1.23")
    val a = c.nextDouble
    assert(a == -1.23)
  }
  "'1 sa -1.23' -> nextLong,nextString,nextDouble" should "generate 1 'sa' -1.23" in {
    val c = generateConsole("1 sa -1.23")
    val a = c.nextLong
    val b = c.nextString
    val d = c.nextDouble
    assert(a == 1)
    assert(b == "sa")
    assert(d == -1.23)
  }
  "'abc' -> readLine" should "generate 'abc'" in {
    val c = generateConsole("abc")
    val a = c.readLine
    assert(a == "abc")
  }
  "'abc def 1' -> readLine" should "generate 'abc def 1'" in {
    val c = generateConsole("abc def 1")
    val a = c.readLine
    assert(a == "abc def 1")
  }
  "'abc def\n123 456' -> readLine" should "generate 'abc def' '123 456'" in {
    val c = generateConsole("abc def\n123 456")
    val a = c.readLine
    val b = c.readLine
    assert(a == "abc def")
    assert(b == "123 456")
  }
  "'abc def\r\n123 456' -> readLine" should "generate 'abc def' '123 456'" in {
    val c = generateConsole("abc def\r\n123 456")
    assert(c.readLine() == "abc def")
    assert(c.readLine() == "123 456")
  }
  "' a \n b ' -> readLine" should "generate ' a ' and ' b '" in {
    val c = generateConsole(" a \n b ")
    val a = c.readLine
    val b = c.readLine
    assert(a == " a ")
    assert(b == " b ")
  }
  "abcde\n25' -> readString -> readInt" should "generate 'abcde' and 25" in {
    val c = generateConsole("abcde\n25")
    assert(c.nextString() == "abcde")
    assert(c.nextInt()    == 25)
  }
  "' a \n b ' -> getString -> readLine" should "generate 'a' and ' b '" in {
    val c = generateConsole(" a \n b ")
    val a = c.nextString
    val b = c.readLine
    assert(a == "a")
    assert(b == " b ")
  }
  "' a\n b ' -> getString -> readLine" should "generate 'a' and ' b '" in {
    val c = generateConsole(" a\n b ")
    val a = c.nextString
    val b = c.readLine
    assert(a == "a")
    assert(b == " b ")
  }
  "'\n' -> readLine" should "generate ''" in {
    val c = generateConsole("\n")
    assert(c.readLine() == "")
  }
  "'\n\n' -> readLine -> readLine" should "generate '', ''" in {
    val c = generateConsole("\n\n")
    assert(c.readLine() == "")
    assert(c.readLine() == "")
  }
  "'g\n\n\n' -> getString -> readLine -> readLine" should "generate 'g' and '', ''" in {
    val c = generateConsole("g\n\n\n")
    assert(c.nextString() == "g")
    assert(c.readLine() == "")
    assert(c.readLine() == "")
  }
  "'1\n\n\n' -> getInt -> readLine -> readLine" should "generate '1' and '', ''" in {
    val c = generateConsole("1\n\n\n")
    assert(c.nextInt() == 1)
    assert(c.readLine() == "")
    assert(c.readLine() == "")
  }
  "'123 456\nabcd' -> nextInt -> nextInt -> readLine" should "generate 123 456 abcd" in {
    val c = generateConsole(" 123 456\nabcd")
    val a = c.nextInt
    val b = c.nextInt
    val d = c.readLine
    assert(a == 123)
    assert(b == 456)
    assert(d == "abcd")
  }
  "'123   456   \nabcd' -> nextInt -> nextInt -> readLine" should "generate 123 456 abcd" in {
    val c = generateConsole(" 123 456\nabcd")
    val a = c.nextInt
    val b = c.nextInt
    val d = c.readLine
    assert(a == 123)
    assert(b == 456)
    assert(d == "abcd")
  }
  "'1  \na' -> nextInt -> readLine" should "generate 1 'a'" in {
    val c = generateConsole("1  \na")
    val b = c.nextInt
    val d = c.readLine
    assert(b == 1)
    assert(d == "a")
  }
  "'1  \r\na' -> nextInt -> readLine" should "generate 1 'a'" in {
    val c = generateConsole("1  \na")
    val b = c.nextInt
    val d = c.readLine
    assert(b == 1)
    assert(d == "a")
  }
  "'abc  \na' -> nextString -> readLine" should "generate abc 'a'" in {
    val c = generateConsole("abc  \na")
    val b = c.nextString
    val d = c.readLine
    assert(b == "abc")
    assert(d == "a")
  }

  "'123' -> readIntVector" should "Vector(123)" in {
    val c = generateConsole("123")
    assert(c.readIntVector() == Vector(123))
  }
  "'1\n2' -> readIntVector -> readIntvector" should "Vector(1),Vector(2)" in {
    val c = generateConsole("1\n2")
    assert(c.readIntVector() == Vector(1))
    assert(c.readIntVector() == Vector(2))
  }
  "'1 \n 2' -> readIntVector -> readIntvector" should "Vector(1),Vector(2)" in {
    val c = generateConsole("1 \n 2 ")
    assert(c.readIntVector() == Vector(1))
    assert(c.readIntVector() == Vector(2))
  }
  "'1 \n2' -> readIntVector -> readIntvector" should "Vector(1),Vector(2)" in {
    val c = generateConsole("1 \n2 ")
    assert(c.readIntVector() == Vector(1))
    assert(c.readIntVector() == Vector(2))
  }
  "'123 456' -> readIntVector" should "Vector(123,456)" in {
    val c = generateConsole("123 456")
    assert(c.readIntVector() == Vector(123,456))
  }
  "'  123   456 ' -> readIntVector" should "Vector(123,456)" in {
    val c = generateConsole("  123   456 ")
    assert(c.readIntVector() == Vector(123,456))
  }
  "'  123   456 \n 7 8 9' -> readIntVector -> readIntvector" should "Vector(123,456),Vector(7,8,9)" in {
    val c = generateConsole("  123   456 \n 7 8 9")
    assert(c.readIntVector() == Vector(123,456))
    assert(c.readIntVector() == Vector(7,8,9))
  }
    "'  123   456\r\n 7 8 9' -> readIntVector -> readIntvector" should "Vector(123,456),Vector(7,8,9)" in {
    val c = generateConsole("  123   456\r\n 7 8 9")
    assert(c.readIntVector() == Vector(123,456))
    assert(c.readIntVector() == Vector(7,8,9))
    }


  ("'1 2\n3 4 5 6\n7 8 9 10' -> nextInt -> nextInt  -> readIntvector -> readIntVector" should
     "1, 2, Vector(3 4 5 6), Vector(7 8 9 10)") in {
    val c = generateConsole("1 2\n3 4 5 6\n7 8 9 10")
    assert(c.nextInt() == 1)
    assert(c.nextInt() == 2)
    assert(c.readIntVector() == Vector(3,4,5,6))
    assert(c.readIntVector() == Vector(7,8,9,10))
  }
  "5 2\r\n1 1 2 2 4 -> nextInt -> nextInt -> readIntVector" should "5 2 [1,1,2,2,4]" in {
    val c = generateConsole("5 2\r\n1 1 2 2 4")
    assert(c.nextInt() == 5)
    assert(c.nextInt() == 2)
    assert(c.readIntVector() == Vector(1,1,2,2,4))
  }
  "5\r\n1 -> nextInt -> nextInt" should "5 '1'" in {
    val c = generateConsole("5\r\n1")
    assert(c.nextInt() == 5)
    assert(c.nextInt() == 1)
  }
  "5\r\n1 -> nextInt -> readLine" should "5 '1'" in {
    val c = generateConsole("5\r\n1")
    assert(c.nextInt() == 5)
    assert(c.readLine() == "1")
  }

  "'にほんご' -> nextString" should "'にほんご'" in {
    val c = generateConsole("にほんご")
    assert(c.nextString() == "にほんご")
  }
}
