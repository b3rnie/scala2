
import org.scalatest.FlatSpec

object Tables {
  def all(): Seq[TableQuery[_ <: Table[_]]] = Seq(
    infohashes
  )

  case class Infohashes (
    infohash: String,
    lastSeen: Long
  )
  class InfohashesTable(tag: Tag) extends Table[Infohashes](tag, "infohashes") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def infohash = column[String]("infohash")
    def lastSeen = column[Long]("last_seen")
    def lastSeenIndex = index("last_seen_idx", lastSeen, unique = false)
    def * = (infohash, lastSeen) <> (Infohashes.tupled, Infohashes.unapply)
  }

  val infohashes = TableQuery[InfohashesTable]
}



class DbSpec extends FlatSpec {
  "open simple" should "work" in new Db {
    db.withSession {
      implicit session =>
        createTables(Tables.all())
        //DB.cocktails.ddl.create
        //DB.infohashes.ddl.create
      // DB.infohashes += DB.Infohashes("foo", 123L)
    }
  }
}
