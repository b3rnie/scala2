import bittorrent._
import org.scalatest._

class BencodingSpec extends FlatSpec {

  "A Bencoded file" should "be decoded and encoded to the same thing" in {
    val original_bytes = FileUtils.readFile("/home/bernie/testtorrent.torrent")
    val decoded        = Bencoding.decode(original_bytes)
    val encoded_bytes  = Bencoding.encode(decoded)
    println(original_bytes.size)
    println(encoded_bytes.size)
    var s = new String(encoded_bytes.toArray, "ISO-8859-1")
    FileUtils.writeFile("/home/bernie/testtorrent")(p => {
      p.write(s)
    })
    assert(original_bytes === encoded_bytes)
  }

//  it should "throw NoSuchElementException if an empty stack is popped" in {
//    val emptyStack = new Stack[String]
//    intercept[NoSuchElementException] {
//      emptyStack.pop()
//    }
//  }
}
