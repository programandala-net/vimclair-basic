#!/bin/sh

# vimbas2tap.sh

# Wrapper for vimbas2tap.vim

# By Marcos Cruz (programandala.net)

# This file is part of Vimclair BASIC
# http://programandala.net/en.program.vimclair_basic.html

# Change history
# 2015-02-09: First version.

# Usage:
#   vimbas2tap.sh filename.vimbas

if [ "$#" -lt 1 ]
then
  echo "Convert a Vimclair BASIC source file to a Sinclair BASIC program in a TAP file"
  echo 'Usage:'
  echo "  ${0##*/} sourcefile"
  exit 1
fi

ex $1 -S ~/.vim/plugins/vimclair_basic.vim -c "call Vimbas2tap()" -c "q!"
exit $?

# vim:tw=78:ts=2:sts=2:et:
