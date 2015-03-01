package generic
import org.slf4j.{Logger, LoggerFactory}

trait Logging {
  lazy private val log = LoggerFactory.getLogger("generic")
  def debug(msg : String) = log.debug(msg)
  def info(msg : String) = log.info(msg)
  def warn(msg : String) = log.warn(msg)
  def error(msg: String) = log.error(msg)
}
