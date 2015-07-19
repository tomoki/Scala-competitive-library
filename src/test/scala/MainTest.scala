import org.scalatest.FlatSpec
import org.scalatest.time._
import org.scalatest.concurrent.Timeouts
import java.nio.file.{Paths, Files}

class MainSpec extends FlatSpec with Timeouts {
  def execWithDummyStdio(main : (Array[String]) => Unit, input : String) : String = {
    val istream = new java.io.ByteArrayInputStream(input.getBytes)
    val ostream = new java.io.ByteArrayOutputStream
    Console.withIn(istream) { Console.withOut(ostream) {
        main(Array[String]())
    } }
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
  val timeout = 2000
  for(i <- input_files){
    val o = eraseExtension(i.getPath) + ".out"
    val exist_expected_file = Files.exists(Paths.get(o))
    i.getPath() should o in {
      assert(exist_expected_file)
      val dummy_in  = new String(Files.readAllBytes(Paths.get(i.getPath)))
      val start     = System.currentTimeMillis()
      // not work because typically, program does not contain Threads.sleep or Object.wait
      var dummy_out = ""
      failAfter(Span(timeout, Millis)) {
        dummy_out = execWithDummyStdio(Main.main,dummy_in)
      }
      // val dummy_out = execWithDummyStdio(Main.main,dummy_in)
      val end       = System.currentTimeMillis();
      val expected  = new String(Files.readAllBytes(Paths.get(o)))
      val interval  = end - start;
      println(i.getPath + " -> " + interval + "millis");
      assert(expected.stripLineEnd == dummy_out.stripLineEnd)
    }
  }
}
