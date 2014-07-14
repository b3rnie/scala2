package tracker

import java.util.concurrent.locks.ReentrantReadWriteLock

trait Lock {
  val lock = new ReentrantReadWriteLock
  val w    = lock.writeLock
  def withLock(f : => Unit) : Unit = {
    w.lock()
    try { f } finally { w.unlock() }
  }
}
