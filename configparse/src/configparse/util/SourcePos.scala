package configparse.util

case class SourcePos(
    path: String,
    row: Int,
    col: Int
)

object SourcePos:
  inline given SourcePos = here
  inline def here: SourcePos = ${ hereImpl }

  import scala.quoted.*
  def hereImpl(using qctx: Quotes): Expr[SourcePos] =
    import qctx.reflect.*
    val pos = Position.ofMacroExpansion
    val file = pos.sourceFile.path.toString
    '{
      SourcePos(
        ${ Expr(file) },
        ${ Expr(pos.startLine + 1) },
        ${ Expr(pos.startColumn) }
      )
    }
end SourcePos
