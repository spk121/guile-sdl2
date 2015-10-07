# -*- Autoconf -*-
#
# guile-sdl2 --- FFI bindings for SDL2
# Copyright © 2015 David Thompson <davet@gnu.org>
#
# This file is part of guile-sdl2.
#
# Guile-sdl2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# Guile-sdl2 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with guile-sdl2.  If not, see
# <http://www.gnu.org/licenses/>.

AC_INIT(guile-sdl2, 0.1)
AC_CONFIG_SRCDIR(sdl2)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile sdl2/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

GUILE_PROGS([2.0.9])
PKG_CHECK_MODULES([SDL2], [sdl2])

LIBSDL2="libSDL2"
LIBSDL2_LIBDIR="no"
LIBSDL2_PREFIX="no"

AC_ARG_WITH([libsdl2-prefix],
  [AS_HELP_STRING([--with-libsdl2-prefix=DIR], [search for SDL2 in DIR])],
  [case "$withval" in
    yes|no)
      ;;
    *)
      LIBSDL2="$withval/lib/libSDL2"
      LIBSDL2_PREFIX="$withval"
      LIBSDL2_LIBDIR="$withval/lib"
      ;;
   esac])

dnl Library name suitable for `dynamic-link'.
AC_MSG_CHECKING([for libSDL2 shared library name])
AC_MSG_RESULT([$LIBSDL2])
AC_SUBST([LIBSDL2])
AC_SUBST([LIBSDL2_PREFIX])
AC_SUBST([LIBSDL2_LIBDIR])

AC_OUTPUT
