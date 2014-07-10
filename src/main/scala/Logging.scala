package tracker

import org.slf4j.{Logger, LoggerFactory}

trait Logging {
  lazy private val log = LoggerFactory.getLogger("meh")

  def debug(msg : String) = log.debug(msg)
  def info(msg : String) = log.info(msg)
  def warn(msg : String) = log.warn(msg)
  // to few levels!
}
