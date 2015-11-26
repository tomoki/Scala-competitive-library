import org.scalatest.FlatSpec
import org.scalatest.time._
import org.scalatest.concurrent.{Interruptor, Timeouts}
import java.nio.file.{Paths, Files}
import net.pushl.io.MyConsole

class TestConsole(val _in:   java.io.InputStream,
                  val __out: java.io.OutputStream)
    extends MyConsole(new java.io.BufferedReader(new java.io.InputStreamReader(_in)),
                      new java.io.PrintStream(__out),
                      Console.err) {
  override def read() = {
    val ret = super.read()
    err.print("\u001b[34m" + ret.toChar + "\u001b[0m")
    ret
  }
  override def print(obj: Any) = {
    val str = if(obj == null) "null" else obj.toString
    err.print("\u001b[32m" + str + "\u001b[0m")
    super.print(str)
  }
  override def println() = {
    err.println()
    super.println()
  }
  override def println(obj: Any) = {
    err.println("\u001b[32m" + obj + "\u001b[0m")
    super.println(obj)
  }
  override def printf(text: String, args: Any*) = {
    err.print("\u001b[32m")
    err.printf(text.format(args : _*))
    err.print("\u001b[0m")

    super.printf(text, args)
  }
}

class MainSpec extends FlatSpec with Timeouts {
  def execWithDummyStdio(main : (Array[String]) => Unit, input : String) : String = {
    val istream = new java.io.ByteArrayInputStream(input.getBytes)
    val ostream = new java.io.ByteArrayOutputStream
    val console = new TestConsole(istream, ostream)
    new Solver(console).main()
    console.flush()
    return ostream.toString
  }
  def lsFile(dir : String) : Seq[java.io.File] = {
    new java.io.File(dir).listFiles.filter(_.isFile)
  }
  def getExtension(name : String) : String = {
    val n = name.lastIndexOf(".")
    if(n == -1) ""
    else name.substring(n+1)
  }
  def eraseExtension(name : String) : String = {
    val n = name.lastIndexOf(".")
    name.substring(0,n)
  }

  val test_files_dir_str = "./src/test/resources"
  if(!Files.exists(Paths.get(test_files_dir_str))){
    fail("tests directory not found")
  }
  val input_files = lsFile(test_files_dir_str).filter((k) => getExtension(k.getName) == "in")
  val timeout = 2000l
  for(i <- input_files){
    val o = eraseExtension(i.getPath) + ".out"
    val exist_expected_file = Files.exists(Paths.get(o))
    i.getPath() should o in {
      assert(exist_expected_file)
      val dummy_in  = new String(Files.readAllBytes(Paths.get(i.getPath)))
      val start     = System.currentTimeMillis()
      // not work because typically, program does not contain Threads.sleep or Object.wait
      var dummy_out = ""
      // implicit val defaultInterruptor = ForceInterruptor
      failAfter(Span(timeout, Millis)) {
        dummy_out = execWithDummyStdio(Main.main,dummy_in)
      }
      val end       = System.currentTimeMillis();
      val expected  = new String(Files.readAllBytes(Paths.get(o)))
      val interval  = end - start;
      println(i.getPath + " -> " + interval + "millis");
      assert(expected.stripLineEnd == dummy_out.stripLineEnd)
    }
  }
}
