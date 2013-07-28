#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.


AC_PREREQ([2.69])
AC_INIT([keylog], [0.1], [rayl@mail.com])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([gnu])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h stdint.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_MMAP
AC_CHECK_FUNCS([select])

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
