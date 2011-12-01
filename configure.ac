# configure.ac - Configure script for the greg.
# Copyright (C) 2004, 2005 Free Software Foundation, Inc.
# Written by Maurizio Boriani <baux@member.fsf.org>
#
# This file is part of the greg.
# 
# The greg is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# The greg is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
#
#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.


AC_PREREQ(2.57)
AC_INIT
. $srcdir/GREG-VERSION
AM_INIT_AUTOMAKE($PACKAGE,$VERSION,baux@member.fsf.org)
AM_MAINTAINER_MODE
AC_CONFIG_HEADER([src/config.h])
AC_GNU_SOURCE

maintainer_mode=${enable_maintainer_mode-no}
	

# Checks for programs.
AC_PROG_CC
AC_PROG_CPP
AM_PROG_AS
AC_PROG_LIBTOOL
AC_PATH_PROG([GUILESNARF], [guile-snarf], :)
AC_PATH_PROG([GUILECONFIG], [guile-config], :)
AC_PATH_PROG([TEXI2HTML],[texi2html],:)
AC_PATH_PROG([TEXI2DVI], [texi2dvi], :)
AC_PATH_PROG([MAKEINFO], [makeinfo], :)


AC_HEADER_STDC
AC_HEADER_TIME
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(libc.h limits.h malloc.h memory.h string.h signal.h sys/file.h sys/fcntl.h sys/ioctl.h sys/stropts.h unistd.h utime.h libguile.h errno.h)


# Checks for typedefs, structures, and compiler characteristics.
AC_CANONICAL_HOST
AC_C_CONST
AC_C_VOLATILE


# version config
AC_SUBST(GREG_MAJOR_VERSION)
AC_SUBST(GREG_MINOR_VERSION)
AC_SUBST(GREG_MICRO_VERSION)
AC_SUBST(GREG_INTERFACE)
AC_SUBST(VERSION)


# Checks for library functions.
AC_CONFIG_FILES([Makefile
		 src/Makefile
		 src/ice-9/Makefile
		 test/Makefile
		 doc/Makefile])	 


AC_OUTPUT
