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

**Version 0.6** : Full Java 21 support

Genesis supports Java language features through Java 21:

- **Java 5**: Generics, enums, annotations, varargs, enhanced for loop, autoboxing
- **Java 7**: Try-with-resources, multi-catch, diamond operator, binary literals
- **Java 8**: Lambdas, method references, default/static interface methods, functional interfaces
- **Java 9**: Private interface methods, module-info.java
- **Java 10**: Local variable type inference (`var`)
- **Java 14-16**: Records, text blocks, pattern matching for instanceof
- **Java 17**: Sealed classes
- **Java 21**: Switch expressions, pattern matching in switch, record patterns, unnamed patterns

See [TODO](TODO) for detailed feature tracking.

## Compatibility

Genesis produces functionally equivalent class files to `javac`. Testing against
a large real-world project (644 source files, 942 class files) shows:

- **0 compilation errors** : identical to javac
- **Identical method signatures** : including generics, throws clauses, and bridge methods
- **Bytecode verification passes** : all generated classes are valid

**Minor differences** (all functionally equivalent):
- Genesis uses `ordinal()` for enum switches; javac generates synthetic `$SwitchMap` classes
- Genesis uses `<clinit>` for constant initialization; javac uses `ConstantValue` attributes

## Performance

Genesis is approximately **2x faster** than javac, with parallel compilation enabled by default.

| Compiler | Time (avg of 10) | Speedup |
|----------|------------------|---------|
| Genesis (parallel) | 1.79s | **2.03x** |
| Genesis (-j1) | 1.97s | 1.84x |
| javac | 3.62s | baseline |

*Benchmark: 644 Java source files : 942 class files on macOS*

## Building

Genesis requires a C99 compiler, zlib, and pthreads.

```bash
# Debug build (with symbols)
make

# Release build (optimized)
make release

# Install to /usr/local/bin
sudo make install
```

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
| `-source <version>` | Source language version (default: 17) |
| `-target <version>` | Target bytecode version (default: 17) |
| `-release <version>` | Set source and target to the same version |
| `-g` | Generate debugging information (default) |
| `-g:none` | Do not generate debugging information |
| `-nowarn` | Disable all warnings |
| `-Werror` | Treat warnings as errors |
| `-j[N]` | Use N threads (default: auto-detect, `-j1` for single-threaded) |
| `-verbose` | Print compilation progress |
| `-version` | Display version information |
| `-help` | Print usage information |

## Known Limitations

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
