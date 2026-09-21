# Autoset

Automatic settings for your application. Read configuration from various
formats and places, and map it to scala types.

Autoset aims to make configuring your application get out of the way, so that
you can focus on writing the important parts.

The idea is simple: define the configuration you need as a case class (or many),
then have autoset read it from files, the environment, or the command line args.
If it succeeds, you get a complete and well-typed configuration which won't fail
when accessed at runtime. If it fails, you'll get a helpful report of the
problem. You'll also be able to inspect what configuration your application is
actually using, along with information on where each config value came from.

## Example

[include:demo]

## Features

- [Parsing config](#parsing-config)
  - [Various formats](#various-formats)
    - [Defining your own format](#defining-your-own-format)
  - [Merge configuration from various places](#merge-configuration-from-various-places)
    - [Merge from multiple files and directories](#merge-from-multiple-files-and-directories)
    - [Value directories](#value-directories)
    - [Environment variables, system props, and args](#environment-variables-system-props-and-args)
  - [Show where your app got your config from](#show-where-your-app-got-your-config-from)
- [Mapping to scala case class](#mapping-to-scala-case-class)
  - [Readers](#readers)
  - [Deriving readers for your own types](#deriving-readers-for-your-own-types)
    - [Case classes](#case-classes)
    - [Sealed types and enums](#sealed-types-and-enums)
    - [Unions of string literals](#unions-of-string-literals)
  - [Writing your own reader](#writing-your-own-reader)
  - [Errors and warnings](#errors-and-warnings)
    - [Collecting diagnostics](#collecting-diagnostics)
  - [Config traits](#config-traits)
    - [Readers for types you don't own](#readers-for-types-you-dont-own)
    - [Settings](#settings)

Autoset reads configuration in two steps. First, every source is parsed into a
single intermediate configuration object (an `autoset.Obj`): a tree of objects,
lists and strings in which every value remembers where it came from. Second,
that object is translated into your own Scala types by readers, which is where
strings become `Int`s, `Duration`s and case classes, where defaults are filled
in, and where anything wrong is reported against the origin recorded in the
first step. The two main sections "Parsing config" and "Mapping to scala case
classes" below follow those two steps.

> [!NOTE]
> All examples in this readme are in the `examples/` folder. You can run each
> with `./mill examples.<name of example>`, for example `./mill examples.demo`.

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

[include:formats]

#### Merge configuration from various places

Configuration is rarely in one place: a package ships defaults, an operator
drops in overrides, an orchestrator mounts secrets, and a deployment sets a
couple of environment variables. Every source is merged into one configuration
object, in a fixed order: files and directories first, then value directories,
then environment variables, system properties and finally command line
arguments.

Merging is recursive for objects, so a source which sets one key of an object
leaves the rest of it alone. Any other value, including a list, replaces what
it overrides entirely.

##### Merge from multiple files and directories

[include:dirs]

##### Value directories

[include:valuedirs]

##### Environment variables, system props, and args

[include:overrides]

#### Show where your app got your config from

[include:origins]

### Mapping to scala case class

#### Readers

[include:readers]

#### Deriving readers for your own types

[include:derivation]

#### Writing your own reader

[include:customreaders]

#### Errors and warnings

[include:errors]

#### Config traits

Readers are not global givens: they are members of an object, and the macro
which derives a reader looks them up on the object it was called on. The same
object carries the settings which decide how Scala names are spelled in config
files. Deriving and reading through your own such object — a *config trait* —
therefore gives you one place to define both.

##### Readers for types you don't own

[include:configtraits]

##### Settings

[include:settings]

## Building

This project is built with [mill](https://mill-build.org).

Some common tasks are also exposed in the `scripts/` directory, for example
`./scripts/test` to run all tests.

The library is available for Scala 3 on the JVM and native. It is published to
maven central:

```scala
mvn"io.crashbox::autoset::0.1.0"
```

## Notice about LLM usage

This project was built with the assistance of a large language model. Many
examples and tests were generated. However, the core logic was hand coded
initially and changes driven by specs.
