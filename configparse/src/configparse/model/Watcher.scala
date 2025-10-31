package configparse.model

import java.nio.file.StandardWatchEventKinds as ek
import scala.jdk.CollectionConverters.*

class Watcher(
    paths: Iterable[os.Path],
    onEvent: Set[os.Path] => Unit,
    daemonize: Boolean = false
) extends java.lang.AutoCloseable
    with java.io.Closeable:

  @volatile private var isRunning = true
  private val service = java.nio.file.FileSystems.getDefault().newWatchService()

  private val watchedDirs = collection.mutable.HashSet.empty[os.Path]
  private val watchedPaths = collection.mutable.HashSet.empty[os.Path]

  for path <- paths if !watchedPaths.contains(path) do
    watchedPaths += path

    val dir = if os.isDir(path) then path else path / os.up
    if !watchedDirs.contains(dir) then
      watchedDirs += dir
      dir.toNIO.register(
        service,
        Array[java.nio.file.WatchEvent.Kind[?]](
          ek.ENTRY_CREATE,
          ek.ENTRY_DELETE,
          ek.ENTRY_MODIFY
        )
      )

  private def process(): Unit =
    var key: java.nio.file.WatchKey = null
    while
      key = service.take()
      key != null
    do
      key.reset()
      val paths = scala.collection.mutable.HashSet.empty[os.Path]
      while
        key = service.poll()
        key != null
      do
        val dir = os.Path(key.watchable().asInstanceOf[java.nio.file.Path])
        if watchedPaths.contains(dir) then paths += dir
        else
          for event <- key.pollEvents().asScala do
            val rel =
              os.SubPath(event.context().asInstanceOf[java.nio.file.Path])
            val path = dir / rel
            if watchedPaths.contains(path) then paths += path
        key.reset()
      end while

      if !paths.isEmpty then onEvent(paths.toSet)
    end while
  end process

  private def run(): Unit =
    while isRunning do
      try process()
      catch
        case e: java.nio.file.ClosedWatchServiceException =>
          isRunning = false
        case ex =>
          ex.printStackTrace()

  private val thread = Thread(() => run(), "config-watcher")
  thread.setDaemon(daemonize)
  thread.start()

  def close(): Unit =
    isRunning = false
    service.close()

  def join(): Unit =
    thread.join()
