package configparse.ini

case class ParseException(
  pos: Pos,
  message: String,
  line: String
) extends Exception(message: String) {
  def pretty() = {
    val caret = " " * (pos.col - 1) + "^"
    s"$message\n$pos\n$line\n$caret"
  }
  override def toString(): String = pretty()
}

class MutablePos extends Pos:
  var row = 1
  var col = 1

  def copy() =
    val m = MutablePos()
    m.row = row
    m.col = col
    m

  override def toString: String = s"$row:$col"

/** A basic INI parser, built-in to configparse. */
class Parser(input: java.io.InputStream, visitor: Visitor):

  private val cpos = MutablePos()
  private var char: Int = -1

  // up to the current character (not included)
  private val lineBuffer = new StringBuilder()

  inline private def readChar(): Unit =
    char match
      case '\n' =>
        cpos.col = 1
        cpos.row += 1
        lineBuffer.clear()
      case -1 | '\r' => // invisible chars, do nothing
      case _ =>
        cpos.col += 1
        lineBuffer += char.toChar
    char = input.read()

  readChar()

  private def errorAt(pos: Pos, message: String): Nothing =
    // read until end of line
    while (char != -1 && char != '\n') {
      readChar()
    }
    val line = lineBuffer.result()
    throw new ParseException(pos, message, line)

  private def prettyChar(char: Int) =
    if (char == -1) "EOF"
    else char.toChar match {
      case '\n' => "new line"
      case c => s"'$c'"
    }

  private def expectationError(expected: String*) = {
    errorAt(cpos.copy(), s"Expected ${expected.mkString(" or ")}. Found ${prettyChar(char)}.")
  }

  private def skipSpace() = {
    while (char == ' ' || char == '\t') {
      readChar()
    }
  }

  // utility text buffer
  private val buffer = new StringBuilder

  private def parseSegment(): String = {
    import java.lang.Character

    if (Character.isAlphabetic(char) || Character.isDigit(char) || char == '_' || char == '-') {
      buffer.clear()
      while (Character.isAlphabetic(char) || Character.isDigit(char) || char == '_' || char == '-') {
        buffer += char.toChar
        readChar()
      }
      buffer.result()
    } else {
      expectationError("alphanumeric", "'_'", "'-'")
    }
  }

  private def parseSectionKey(): List[String] = {
    val segs = collection.mutable.ListBuffer.empty[String]
    segs += parseSegment()
    while (char == '.') {
      readChar()
      segs += parseSegment()
    }
    segs.toList
  }

  private def parseSection(): Unit =
    val pos = cpos.copy()

    if char != '[' then expectationError("'['")
    readChar()
    skipSpace()
    val sectionPath = parseSectionKey()
    skipSpace()
    if (char != ']')  expectationError("']'")
    readChar()

    visitor.visitSection(pos, sectionPath)


  private def parseKeyValue(): Unit =
    val pos = cpos.copy()
    val key = parseSegment()

    skipSpace()
    if (char != '=') expectationError("'='")
    readChar()
    skipSpace()

    visitor.visitKey(pos, key)

    val vpos = cpos.copy()
    // parse rhs as plain text until new line
    buffer.clear()
    while (char != -1 && char != '\n') {
      buffer += char.toChar
      readChar()
    }
    val value = buffer.result()

    if value.isEmpty() then
      visitor.visitEmpty(vpos)
    else
      visitor.visitString(vpos, value)

  private def parseNext(): Unit =
    while (char == ' ' || char == '\t' || char == '\n') readChar()
    char match {
      case -1 =>
      case ';' | '#' =>
        while char != -1 && char != '\n' do readChar()
        parse()
      case '[' => parseSection()
      case s => parseKeyValue()
    }

  def parse(): Unit =
    while char != -1 do parseNext()


