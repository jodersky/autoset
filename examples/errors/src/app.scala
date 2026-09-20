package example

// Config is written by people, and people make mistakes. autoset reports them
// the way a compiler does: with the message, and the place in the file it
// applies to.
//
// Everything is read before anything is reported, so one run finds every
// problem rather than one per run. Errors mean the config could not be read,
// and `read` returns `None`. Warnings mean something was suspicious but
// readable — a key nothing expected, or a deprecated one — and the config is
// still returned.

// `config.yaml`, with three mistakes in it
// ```yaml
//include:../config.yaml
// ```

//snippet:start
case class Config(server: Server, db: Db) derives autoset.Reader
case class Server(host: String, port: Int) derives autoset.Reader
case class Db(url: java.net.URI, pool: Int) derives autoset.Reader

def main(args: Array[String]): Unit =
  val file = args.find(!_.startsWith("--")).getOrElse("config.yaml")

  if !args.contains("--collect") then
    // by default, diagnostics are printed to stderr as they are reported
    val (config, _) = autoset.read[Config](
      paths = Seq(os.pwd / file)
    ).getOrElse(sys.exit(1))

    println(config)
//snippet:end
  else
    collect(file)

// Reading the file above reports all three problems at once. Note where each
// one points: the unknown key at the key itself, the mismatch at the value
// that could not be read, and the missing field at the object it should have
// been added to, which is the place you would go to fix it.

/* usage snippet
$ ./app
error: config.yaml:2:3: missing required field 'server.port'
warning: config.yaml:3:9: unknown key 'server.prot'
error: config.yaml:7:9: expected an integer for 'db.pool', found 'many'
*/

// A file which does not parse at all is reported by the parser instead, which
// can show the offending line:

/* usage snippet
$ ./app broken.json
error: broken.json:2:43: expected json value got "}"
  "server": {"host": "localhost", "port": },
                                          ^
*/

// ##### Collecting diagnostics
//
// Printing as you go is the default. Pass a plain `autoset.Reporter()` to
// collect diagnostics instead, and do what you like with them once loading is
// done — count them, render them somewhere other than stderr, or turn them into
// log records.

//snippet:start
def collect(file: String) =
  val reporter = autoset.Reporter()

  val result = autoset.read[Config](
    paths = Seq(os.pwd / file),
    reporter = reporter
  )

  println(s"${reporter.errors} error(s), ${reporter.warnings} warning(s)")
  for d <- reporter.diagnostics do
    println(s"  ${d.severity}: ${d.origin.fold("")(_.pretty + ": ")}${d.message}")

  result match
    case Some((config, _)) => println(config)
    case None => sys.exit(1)
//snippet:end

/* usage snippet
$ ./app --collect
2 error(s), 1 warning(s)
  Error: config.yaml:2:3: missing required field 'server.port'
  Warning: config.yaml:3:9: unknown key 'server.prot'
  Error: config.yaml:7:9: expected an integer for 'db.pool', found 'many'
*/
