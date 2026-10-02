# Genesis

A fast, minimal Java compiler written in C.

## Overview

Genesis is a from-scratch Java compiler implementation designed for speed and
simplicity. Written in portable C99 with minimal dependencies, it aims to be
a lightweight alternative to `javac` while supporting all core Java language
features.

### Design Goals

- **Small**: Minimal codebase, no external dependencies beyond zlib and pthreads
- **Fast**: Direct compilation without intermediate representations
- **Correct**: Full Java language specification compliance
- **Portable**: Standard C99, builds on Linux and macOS

## Current Status

**Version 1.0.0** : Full Java 25 support

Genesis supports Java language features through Java 25:

- **Java 5**: Generics, enums, annotations, varargs, enhanced for loop, autoboxing
- **Java 7**: Try-with-resources, multi-catch, diamond operator, binary literals
- **Java 8**: Lambdas, method references, default/static interface methods, functional interfaces
- **Java 9**: Private interface methods, module-info.java
- **Java 10**: Local variable type inference (`var`)
- **Java 14-16**: Records, text blocks, pattern matching for instanceof
- **Java 17**: Sealed classes
- **Java 21**: Switch expressions, pattern matching in switch, record patterns, unnamed patterns
- **Java 22**: Unnamed variables and patterns (`_`)
- **Java 25**: Module imports, compact source files and instance main, flexible constructor bodies

Preview features (e.g. primitive patterns) and withdrawn string templates are not supported.

See [TODO](TODO) for detailed feature tracking.

## Compatibility

Genesis produces class files that are functionally equivalent to `javac`'s.
It is tested against [gumdrop](https://github.com/cpkb-bluezoo/gumdrop), a
large real-world project (1408 source files, about 340,000 lines, plus 552
JUnit test sources):

- **0 compilation errors** : identical to javac
- **The same classes** : javac writes 2767 class files for the main sources and
  genesis 2726; the 41 it leaves out are javac's synthetic enum-switch holders
  (see below)
- **The test suite passes** : gumdrop's JUnit suite (519 suites, 6496 tests),
  compiled entirely by genesis and run on the JVM with bytecode verification,
  passes without a failure (`make gumdrop-smoke`)

**Minor differences** (all functionally equivalent):
- Genesis lowers enum `switch` with `ordinal()` and `lookupswitch` instead of emitting
  javac's synthetic `$SwitchMap` holder classes (`Outer$N`). That skips extra class
  files and `<clinit>` work during compilation, which fits the goal of a fast compiler.
- Genesis uses `<clinit>` for constant initialization; javac uses `ConstantValue` attributes

## Performance

Compiling all of [gumdrop](https://github.com/cpkb-bluezoo/gumdrop) in one
invocation (1408 source files, about 340,000 lines, 15 jars on the classpath,
`--release 25 -g`, output to an empty directory), genesis against javac 25.0.4
on an Apple M4 (4 performance and 6 efficiency cores), average of 10 runs:

| Compiler | Wall clock | CPU time (user+sys) | Speedup vs javac |
|----------|-----------:|--------------------:|-----------------:|
| genesis (default, one job per processor) | 0.41 s | 2.64 s | 6.8x |
| genesis `-j1` | 1.05 s | 1.03 s | 2.7x |
| javac | 2.79 s | 11.74 s | 1.0x |

By default genesis parses on all processors, then divides semantic analysis
and code generation between worker processes, one per processor. `-j1` does
everything in a single thread of one process and uses the least processor
time in total; `-jN` limits the number of jobs to N. Batches of fewer than 32
files, and JAR output, are analysed in the compiler process itself.

Run `make bench` (see CONTRIBUTING) for numbers on your own machine.

## Building

Genesis requires a C99 compiler, zlib, and POSIX threads.

From a release tarball:

```bash
./configure
make
sudo make install
```

From a git checkout, generate the build system first (needs Autoconf 2.69+
and Automake 1.13+):

```bash
autoreconf -fi
./configure
make
```

Regression tests (`make check`) and the jtreg integration live in `test/`,
which is only present in a git checkout, not in the release tarball. They
need a Java 21+ runtime: set `JAVA` or `JAVA_HOME`.

## Usage

```bash
# Compile a single file
genesis Hello.java

# Compile with output directory
genesis -d classes/ src/Main.java

# Create an executable JAR
genesis -jar app.jar -main-class com.example.Main src/Main.java

# Compile with classpath and sourcepath
genesis -cp lib/util.jar -sourcepath src/ -d out/ src/Main.java

# Verbose output
genesis -verbose Hello.java

# Show version
genesis -version
```

### Command Line Options

| Option | Description |
|--------|-------------|
| `-d <dir>` | Output directory for class files |
| `-jar <file>` | Output to JAR file instead of directory |
| `-main-class <class>` | Specify main class for JAR manifest |
| `-cp <path>` / `-classpath <path>` | Classpath for dependency resolution |
| `-sourcepath <path>` | Source path for finding source files |
| `-source <version>` | Source language version (default: 25) |
| `-target <version>` | Target bytecode version (default: automatic) |
| `-release <version>` | Set source and target to the same version |
| `-g` | Generate debugging information (default) |
| `-g:none` | Do not generate debugging information |
| `-nowarn` | Disable all warnings |
| `-Werror` | Treat warnings as errors |
| `-j[N]` | Run N jobs at once (default: one per processor; `-j1` compiles in a single thread of one process) |
| `-verbose` | Print compilation progress |
| `-version` | Display version information |
| `-help` | Print usage information |

## Known Limitations

- **Nested classes and `-target` below 11**: nested classes reach each other's
  private members via nestmates, which need class file version 55 (Java 11).
  With an explicit `-target` below 11, private access between nested classes
  fails at run time. Genesis does not yet generate `access$N` accessors.

- **UTF-only source encoding**: Source files must be valid UTF-8, UTF-16, or
  UTF-32 (with BOM for UTF-16/32). Unlike `javac`, genesis does not attempt to
  guess or fall back to platform-default encodings. Invalid UTF-8 sequences
  are rejected with an error. Use Unicode escapes (`\uXXXX`) for non-UTF-8 sources.

## Contributing

See [CONTRIBUTING](CONTRIBUTING) for coding standards and guidelines.

## License

Genesis is free software, released under the
[GNU General Public License](COPYING) version 3 or later.

Copyright © 2016, 2020, 2026 Chris Burdess
