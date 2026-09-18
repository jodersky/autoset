package autoset

/** Marks a case class field as secret: its value is shown as `<secret>`
  * instead of its actual contents.
  */
class secret extends scala.annotation.StaticAnnotation
