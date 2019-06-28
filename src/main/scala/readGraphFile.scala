import java.io.File
import sbt.io.IO
class readGraphFile(fileName: String) {
  def readFile: String =
    IO.read(new File(fileName))
}