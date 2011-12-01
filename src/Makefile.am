#
#  Makefile for Greg Library and control program
#  
#  Copyright (C) 1998, 2001, 2003, 2005 Free Software Foundation, Inc.
#
#  Written by:	Richard Frith-Macdonald <rfm@gnu.org>
#  Modifed by:  Nicola Pero <n.pero@mi.flashne.it>
#  Modifed by:  Maurizio Boriani <baux@member.fsf.org>
# 
#  This file is part of Greg.
#
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#  
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#  
#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#


AUTOMAKE_OPTIONS = gnu 
SUBDIRS = ice-9

BUILT_SOURCES = greg.x

SUFFIXES = .x
.c.x:
	$(GUILESNARF) $(DEFS) $(INCLUDES) $(CPPFLAFS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }


lib_LTLIBRARIES = libgreg.la

libgreg_la_SOURCES = greg.c greg.x
libgreg_la_LIBADD = -ldl -lz -lguile
libgreg_la_LDFLAGS = -export-dynamic -version-info @GREG_INTERFACE@
libgreg_la_DEPENDENCIES = $(LTLIBOBJS)

bin_SCRIPTS = greg

EXTRA_DIST = $(bin_SCRIPTS)

CLEANFILES = .log 