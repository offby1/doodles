#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([anagrams], [0.0], [offby1@blarg.net])
AM_INIT_AUTOMAKE()
AC_CONFIG_SRCDIR([anagrams.cc])
AC_CONFIG_HEADER([config.h])

# Checks for programs.

AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB

# Checks for libraries.
AC_CHECK_LIB([gmp], [__gmpz_init], [], [AC_MSG_ERROR([You're missing the nifty gmp library.])])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE

# Checks for library functions.
AC_CHECK_FUNCS([regcomp strerror], [], [AC_MSG_ERROR([You're missing some important library function or other.])])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
