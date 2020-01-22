import org.scalatest.FlatSpec
import reader.IniReader

class IniReaderSpec extends FlatSpec {

  it should "load file" in {
    val data = IniReader.read("ini/sample.ini")

    assert(data.size == 2)

    assert(data.head._1 == "PHP")
    assert(data.head._2.size == 2)
    assert(data.head._2.last == ("short_open_tag", "Off"))

    assert(data.last._1 == "MySQL")
    assert(data.last._2.size == 3)
    // TODO: need to apply checks like this to convert int value to INT type and so on
    assert(data.last._2.last == ("mysql.cache_size", "2000"))
  }

  it should "load file php.ini file" in {
    val data = IniReader.read("ini/php.ini_Default.ini")

    assert(data.nonEmpty)

    // random section keys check
    assert(data.exists(_._1 == "Session"))
    assert(data.exists(_._1 == "browscap"))
    assert(data.exists(_._1 == "mail function"))

    val use_cookies = data.filter(_._1 == "Session").head._2.filter(_._1 == "session.use_cookies").head
    assert(use_cookies._1 == "session.use_cookies")
    assert(use_cookies._2 == "1")
  }
}
