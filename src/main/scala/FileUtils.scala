package tracker

import scala.io.{Source}
import java.io._

object FileUtils {
  def readFile(name : String) : List[Byte] = {
    val src = Source.fromFile(name)(scala.io.Codec.ISO8859)
    try {
      src.map(_.toByte).toList
    } finally { src.close }
  }

  def writeFile(f : String)(op : java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new File(f), "ISO-8859-1")
    try {
      op(p)
    } finally { p.close() }
  }
}
