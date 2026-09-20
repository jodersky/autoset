package example

// Everything used so far — `autoset.read`, `autoset.Reader`, the given readers
// for primitives and collections — is a member of a single object,
// `autoset.default`, which `autoset.<name>` forwards to. `Reader` is a type
// *inside* that object, so a `given` written inside another such object is
// scoped to it: it does not leak into the rest of the program, and it does not
// have to go into the companion of the type it reads.
//
// So: make your own object, put the readers your application needs in it, and
// read through it.

// `config.yaml`
// ```yaml
//include:../config.yaml
// ```

//snippet:start
object cfg extends autoset.Api with autoset.DefaultReaders:

  // `java.util.Locale` is not ours, so we cannot put a reader in its
  // companion object. Here it is simply a member of `cfg`
  given Reader[java.util.Locale] with
    def read(value: autoset.Value, base: Option[java.util.Locale], ctx: autoset.model.Context) =
      value match
        case autoset.Str(raw, _, _) =>
          java.util.Locale.forLanguageTag(raw.trim) match
            case l if l.getLanguage.isEmpty =>
              autoset.ReaderUtils.mismatch("a language tag (e.g. 'de-CH')", value, ctx)
            case l => Some(l)
        case _ => autoset.ReaderUtils.mismatch("a language tag (e.g. 'de-CH')", value, ctx)

    override def show(a: java.util.Locale) =
      Some(autoset.Str(a.toLanguageTag, autoset.LitKind.String, Nil))

  given Reader[java.nio.charset.Charset] with
    def read(value: autoset.Value, base: Option[java.nio.charset.Charset], ctx: autoset.model.Context) =
      value match
        case autoset.Str(raw, _, _) if java.nio.charset.Charset.isSupported(raw.trim) =>
          Some(java.nio.charset.Charset.forName(raw.trim))
        case _ => autoset.ReaderUtils.mismatch("a charset (e.g. 'UTF-8')", value, ctx)

    override def show(a: java.nio.charset.Charset) =
      Some(autoset.Str(a.name, autoset.LitKind.String, Nil))
//snippet:end

// Derive with `cfg.Reader` rather than `autoset.Reader`, so that the macro
// looks readers up in `cfg`. Deriving with `autoset.Reader` here would fail:
// `autoset.default` has never heard of a `Locale`.

//snippet:start
case class Config(
  name: String, // read with cfg.StringReader, inherited from DefaultReaders
  locale: java.util.Locale, // read with the given above
  charset: java.nio.charset.Charset
) derives cfg.Reader

@main
def run() =
  // and read through `cfg` too, not through `autoset`
  val (config, raw) = cfg.read[Config](
    paths = Seq(os.pwd / "config.yaml")
  ).getOrElse(sys.exit(1))

  println(config)
//snippet:end

/* usage snippet
$ ./app
Config(example,de_CH,UTF-8)
*/

// One object holding every reader an application needs is also just a good
// place to look: there is one answer to "how is this type read", and it is not
// scattered over companion objects. The next section uses the same object for
// the other half of the pattern, the settings.
