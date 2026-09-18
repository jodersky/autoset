package autoset.derivation

/** All readers: primitives and standard library types, collections, and
  * derived readers for case classes, sealed types and enums with `readerFor`.
  */
trait DefaultReaders extends BaseReaders with CompositeReaders with DerivedReaders
