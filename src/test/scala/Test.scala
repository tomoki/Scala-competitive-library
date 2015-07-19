// This file is intended to be used for problem-specific test cases.
import org.scalatest.FunSuite

class TestSpec extends FunSuite {
  import net.pushl.io.MyConsole
  val console = new MyConsole(Console.in, Console.out, Console.err)
  val solver  = new Solver(console)

  // example
  // test("1 = 1") {
  //   assert(1 == 1)
  // }
}
