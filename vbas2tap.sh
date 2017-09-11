#!/bin/sh

# vbas2tap.sh

# Command line wrapper for
# ~/.vim/plugins/vbas2tap.vim

# By Marcos Cruz (programandala.net)

# This file is part of Vimclair BASIC
# http://programandala.net/en.program.vimclair_basic.html

# ##############################################################
# Usage

#   vbas2tap.sh filename.vbas

# ##############################################################
# History

# 2015-02-09: First version.
# 2015-02-10: File error checking.
# 2015-02-11: Vim options are improved.
# 2017-09-12: Fix typo.

# ##############################################################
# To-do

# Options for all the converter's config, and also:
# -v --version ('grep' the source)
#
# 2015-03-17: Use ':runtime' command instead of '-S' option.

# ##############################################################
# Error checking

if [ "$#" -ne 1 ] ; then
  echo "Convert a Vimclair BASIC source file to a Sinclair BASIC program in a TAP file"
  echo 'Usage:'
  echo "  ${0##*/} sourcefile"
  exit 1
fi

if [ ! -e "$1"  ] ; then
  echo "<$1> does not exist"
  exit 1
fi

if [ ! -f "$1"  ] ; then
  echo "<$1> is not a regular file"
  exit 1
fi

if [ ! -r "$1"  ] ; then
  echo "<$1> can not be read"
  exit 1
fi

if [ ! -s "$1"  ] ; then
  echo "<$1> is empty"
  exit 1
fi

# ##############################################################
# Main

# Vim options used:
# -e = Enter Vim in ex mode (in this case, the goal is just
#      preventing Vim from clearing the screen).
# -n = No swap file will be used. This makes it possible
#      to convert a file currently open by other instance of Vim,
#      without asking the user for confirmation.
# -s = Silent mode (does not affect BAS2TAP messages).
# -S = Vim file to be sourced after the first file has been read.
# -c = Vim command to be executed after the first file has been read.

vim -e -n -S ~/.vim/vbas2tap.vim -c "call Vbas2tap() | q!" $1
exit $?

# vim:tw=78:ts=2:sts=2:et:
