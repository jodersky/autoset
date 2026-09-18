package autoset.parsers

import autoset.model.Origin
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.Searching.{Found, InsertionPoint}

/** An input stream that keeps a copy of everything read through it, so that
  * byte offsets reported by a parser can be turned into positions and source
  * lines.
  *
  * Configuration files are small, so keeping them in memory is fine.
  */
private[parsers] class LocationStream(in: java.io.InputStream, sizeHint: Int)
    extends java.io.InputStream:
  private var buf = new Array[Byte](if sizeHint > 0 then sizeHint else 1024)
  private var size = 0
  private val lineStarts = collection.mutable.ArrayBuffer[Int](0)

  private def record(b: Array[Byte], off: Int, len: Int): Unit =
    if size + len > buf.length then
      buf = java.util.Arrays.copyOf(buf, math.max(buf.length * 2, size + len))
    var i = 0
    while i < len do
      val c = b(off + i)
      buf(size) = c
      size += 1
      if c == '\n' then lineStarts += size
      i += 1

  override def read(): Int =
    val c = in.read()
    if c != -1 then record(Array(c.toByte), 0, 1)
    c

  override def read(b: Array[Byte], off: Int, len: Int): Int =
    val n = in.read(b, off, len)
    if n > 0 then record(b, off, n)
    n

  /** 0-based index of the line containing byte `idx`. */
  private def lineIndex(idx: Int): Int = lineStarts.search(idx) match
    case Found(i) => i
    case InsertionPoint(i) => i - 1

  /** The origin of the byte at offset `idx`. The column counts characters, not
    * bytes.
    */
  def origin(name: String, idx: Int): Origin.File =
    val i = idx.max(0).min(size)
    val l = lineIndex(i)
    val start = lineStarts(l)
    val col = String(buf, start, i - start, UTF_8).length + 1
    Origin.File(name, i, l + 1, col)

  /** The text of the line containing byte `idx`, without its line terminator. */
  def line(idx: Int): String = lineText(lineIndex(idx.max(0).min(size)) + 1)

  /** The origin of the start of the 1-based `line`, for parsers that don't
    * report columns. Unknown if the line doesn't exist.
    */
  def lineOrigin(name: String, line: Int): Origin.File =
    if line < 1 || line > lineStarts.size then Origin.File(name, -1, -1, -1)
    else Origin.File(name, lineStarts(line - 1), line, -1)

  /** The text of the 1-based `line`, without its line terminator, or "" if it
    * doesn't exist.
    */
  def lineText(line: Int): String =
    if line < 1 || line > lineStarts.size then ""
    else
      val start = lineStarts(line - 1)
      var end = if line < lineStarts.size then lineStarts(line) - 1 else size
      if end > start && buf(end - 1) == '\r' then end -= 1
      String(buf, start, end - start, UTF_8)

  /** Read the rest of the input, so that all lines are available. */
  def drain(): Unit =
    val skip = new Array[Byte](4096)
    while read(skip, 0, skip.length) != -1 do ()
