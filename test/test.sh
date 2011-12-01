#!/bin/sh


# test.sh -- prepare the env and execute a self test
# Copyright (C) 1998, 2000, 2001, 2003, 2005 Free Software Foundation, Inc.
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
#                                               -*- Makefile -*-
# Process this file with autoconf to produce a configure script.


export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:../src/.libs
export GUILE_LOAD_PATH=$GUILE_LOAD_PATH:../src/
guile -s ../src/greg

