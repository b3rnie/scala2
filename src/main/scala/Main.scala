package Bencode

object Main {
  def main(args: Array[String]) {
    val bytes = FileUtils.readFile("/home/bernie/testtorrent.torrent")
    var (res, tl) = Bencode.decode(bytes)
    println(tl.size)
    var res2 = Bencode.encode(res)
    println(res2.size)
    var s = new String(res2.toArray, "ISO-8859-1")
    FileUtils.writeFile("/home/bernie/testtorrent")(p => {
      p.write(s)
    })
  }
}
