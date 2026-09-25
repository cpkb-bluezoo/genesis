dnl configure.ac - Autoconf input for genesis
dnl Copyright (C) 2016, 2020, 2026 Chris Burdess <dog@gnu.org>
dnl
dnl This file is part of genesis.
dnl
dnl genesis is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl genesis is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, see <https://www.gnu.org/licenses/>.

AC_PREREQ([2.69])
AC_INIT([genesis], [1.0.0], [dog@gnu.org], [genesis],
        [https://github.com/cpkb-bluezoo/genesis])
AC_CONFIG_SRCDIR([src/genesis.c])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([1.13 gnu std-options subdir-objects dist-xz no-dist-gzip
                  -Wall -Wno-portability])
AM_SILENT_RULES([yes])

dnl Genesis is written in C99.  Newer Autoconf selects C11 or later by
dnl default, which would hide accidental use of post-C99 features, so pin the
dnl language level to C99 when the compiler takes -std=c99.
AC_USE_SYSTEM_EXTENSIONS
AC_PROG_CC
AC_CACHE_CHECK([whether $CC accepts -std=c99], [genesis_cv_std_c99],
  [genesis_save_CC=$CC
   CC="$CC -std=c99"
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdbool.h>]],
       [[for (int i = 0; i < 1; i++) { bool b = true; (void) b; }]])],
     [genesis_cv_std_c99=yes], [genesis_cv_std_c99=no])
   CC=$genesis_save_CC])
AS_IF([test "x$genesis_cv_std_c99" = xyes], [CC="$CC -std=c99"],
      [AC_MSG_ERROR([a C99 compiler is required])])
AC_PROG_INSTALL

dnl Standard C99 headers plus the POSIX ones we rely on
AC_CHECK_HEADERS([unistd.h dirent.h fcntl.h sys/stat.h sys/types.h],
                 [], [AC_MSG_ERROR([required POSIX header not found])])

dnl POSIX threads (parallel compilation)
AC_SEARCH_LIBS([pthread_create], [pthread], [],
               [AC_MSG_ERROR([POSIX threads are required])])

dnl zlib (JAR output)
AC_CHECK_HEADER([zlib.h], [], [AC_MSG_ERROR([zlib.h not found; install the zlib development files])])
AC_CHECK_LIB([z], [deflate], [], [AC_MSG_ERROR([zlib is required])])

AC_CHECK_FUNCS([strdup strndup sysconf], [],
               [AC_MSG_ERROR([required C library function not found])])

dnl Compiler warnings (maintainers may override with CFLAGS)
AC_ARG_ENABLE([warnings],
  [AS_HELP_STRING([--disable-warnings], [do not add -Wall -Wextra -pedantic])],
  [], [enable_warnings=yes])
AS_IF([test "x$enable_warnings" = xyes],
      [AS_IF([test "x$GCC" = xyes || $CC --version 2>&1 | grep -qi clang],
             [WARN_CFLAGS="-Wall -Wextra -pedantic"])])
AC_SUBST([WARN_CFLAGS])

dnl Development-only pieces (regression tests, jtreg, parity scripts) live in
dnl test/ and are deliberately not part of the distribution tarball.
AM_CONDITIONAL([HAVE_TESTS], [test -f "$srcdir/test/run-tests.sh"])

dnl Java runtime used by "make check" to execute compiled test classes
AC_ARG_VAR([JAVA], [Java runtime used by make check])
AS_IF([test -z "$JAVA" && test -n "$JAVA_HOME" && test -x "$JAVA_HOME/bin/java"],
      [JAVA="$JAVA_HOME/bin/java"])
AS_IF([test -z "$JAVA"], [AC_PATH_PROG([JAVA], [java])])

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
