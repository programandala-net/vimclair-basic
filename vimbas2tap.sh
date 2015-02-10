#!/bin/sh

# Usage:
#   vimbas2tap.sh filename.vimbas

ex $1 -S vimclair_basic.vim -c "call VimclairBASIC()" -c "q!"
