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
  "' a \n b ' -> readLine" should "generate ' a ' and ' b '" in {
    val c = generateConsole(" a \n b ")
    val a = c.readLine
    val b = c.readLine
    assert(a == " a ")
    assert(b == " b ")
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
  "'abc  \na' -> nextString -> readLine" should "generate abc 'a'" in {
    val c = generateConsole("abc  \na")
    val b = c.nextString
    val d = c.readLine
    assert(b == "abc")
    assert(d == "a")
  }
}
