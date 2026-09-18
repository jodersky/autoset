package configparse.ini

trait Pos:
  def row: Int
  def col: Int

trait Visitor:
  def visitKey(pos: Pos, key: String): Unit
  def visitString(pos: Pos, text: String): Unit
  def visitEmpty(pos: Pos): Unit
  def visitSection(pos: Pos, sectionKey: Seq[String]): Unit

object NoopVisitor extends Visitor:
  def visitKey(pos: Pos, key: String): Unit = ()
  def visitString(pos: Pos, text: String): Unit = ()
  def visitEmpty(pos: Pos): Unit = ()
  def visitSection(pos: Pos, sectionKey: Seq[String]): Unit = ()
