import bittorrent._
import org.scalatest._
import scala.util.Random

class DHTSpec extends FlatSpec {
  "Distance between nodes" should "be calculated correctly" in {
    assert(DHT.distance(Array[Byte](0x00, 0x01),
                        Array[Byte](0x02, 0x00)) === 513)
    assert(DHT.distance(Array[Byte](1,2,3,4,5),
                        Array[Byte](1,2,3,4,5)) === 0)
    val rand_id0 = new Array[Byte](20)
    val rand_id1 = new Array[Byte](20)
    Random.nextBytes(rand_id0)
    Random.nextBytes(rand_id1)
    
    val s0 = new String(rand_id0.map(_.toChar))
    val s1 = new String(rand_id1.map(_.toChar))

    val dist0 = DHT.distance(rand_id0,rand_id1)
    println("dist0 = " + dist0)
    val dist1 = DHT.distance(s0.toArray.map(_.toByte),
                             s1.toArray.map(_.toByte))
    
    println("dist1 = " + dist1)
    // 2^160
    var bint = BigInt(1)
    println(bint << 160)
    // 2^159
    bint = 1
    println(bint << 159)
  }
}
