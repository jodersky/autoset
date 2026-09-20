# Autoset

Read configuration from multiple formats, and map it to scala types.

## Example

[include:demo]

## Features

### Parsing config

#### Various formats

The following formats are provided out-of-the-box:

- YAML (via https://github.com/jodersky/yamlesque)
- INI (built-in parser)
- JSON (via https://github.com/com-lihaoyi/upickle)
- HOCON (aka the typesafe/lightbend config library)
- JVM system properties
- env vars
- arguments

A user can also define their own formats, by implementing a parser.

TODO: example showing various formats

#### Merge configuration from various places

##### Merge from multiple directories

TODO: include example which shows how files are merged from directories,

##### varDirs

TODO: example showing varDirs

##### Environment variables, system props, and args

TODO an example showing how to override with these

#### Show where your app got your config from

TODO example showing origins, emphasis on tracking origin as well as secret values hidden

### Mapping to scala case class

#### Readers

TODO: example showing common reader types, primitives and composite
TODO: example describing built-in case class and union type readers
TODO: example showing how to build a custom reader

#### Errors and warnings

TODO example showing errors (e.g. missing field), as well as warnings (e.g. unknown field).

#### Config traits

TODO: mention that autoset uses a "config trait" pattern, where implicit readers are looked up based on path and settings
also. Then showcase this pattern with two examples:

- TODO: example showing how to define readers in an object extends Api, and relying on the path-dependent implicit lookup

- TODO: example showing how to override common settings such as mapping field names to snake_case and changing the descriminator
