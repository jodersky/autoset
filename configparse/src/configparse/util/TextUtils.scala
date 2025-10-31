package configparse.util

import scala.collection.mutable.ArrayBuilder
import java.util.Arrays

object TextUtils:

  class LocationStream(in: java.io.InputStream, sizeHint: Int) extends java.io.InputStream:
    var i = 0
    val offsets = collection.mutable.ArrayBuffer[Int](sizeHint)

    offsets += 0
    i += 1

    override def read(): Int =
      val c = in.read()
      if c == -1 then return -1
      if c == '\n' then offsets += i
      i += 1
      c

    // TODO: make this a binary search
    def posToRowAndCol(pos: Int): (Int, Int) =
      var idx = 0
      var offset = 0
      while
        idx < offsets.size && offsets(idx) < pos
      do
        offset = offsets(idx)
        idx += 1

      // var idx = Arrays.binarySearch(offsets.toArray, pos)
      // if idx < 0 then idx = -idx - 2
      // val offset = offsets(idx)

      val row = idx - 1
      val col = pos - offset + 1
      // println(s">>> pos: ${pos}, idx: ${idx}, offset: ${offset}, col: ${col}")
      (row, col)


  def lineOffsets(in: java.io.InputStream): Array[Int] =
    var i = 0
    var c: Int = -1
    val offsets = ArrayBuilder.ofInt()

    c = in.read()
    if c != -1 then
      offsets += 0
      i += 1

    while
      c = in.read()
      c != -1
    do
      if c == '\n' then
        offsets += i
      i += 1

    offsets.result()

  def lineOffsets(readable: geny.Readable): Array[Int] =
    readable.readBytesThrough(lineOffsets(_))

  // Lookup row and column from a position. Complexity: O(log(rows))
  def posToRowAndCol(offsets: Array[Int], pos: Int): (Int, Int) =
    var idx = Arrays.binarySearch(offsets, pos)
    if idx < 0 then idx = -idx - 2
    val offset = offsets(idx)

    val row = idx + 1
    val col = pos - offset
    // println(s">>> pos: ${pos}, idx: ${idx}, offset: ${offset}, col: ${col}")
    (row, col)

  /** `thisIsKebabCase => this-is-kebab-case` */
  def kebabify(camelCase: String): String = {
    val kebab = new StringBuilder
    var prevIsLower = false
    for (c <- camelCase) {
      if (prevIsLower && c.isUpper) {
        kebab += '-'
      }
      kebab += c.toLower
      prevIsLower = c.isLower
    }
    kebab.result()
  }

  /** `thisIsSnakeCase => this_is_snake_case` */
  def snakify(camelCase: String): String = {
    val snake = new StringBuilder
    var prevIsLower = false
    for (c <- camelCase) {
      if (prevIsLower && c.isUpper) {
        snake += '_'
      }
      snake += c.toLower
      prevIsLower = c.isLower
    }
    snake.result()
  }
