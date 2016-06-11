# Vimclair BASIC Makefile
# By Marcos Cruz (programandala.net)
#
# This file is part of Vimclair BASIC
# (http://programandala.net/en.program.vimclair_basic.html)

################################################################
# Change log

# 2015-02-12: First version.

################################################################
# Notes

# $? list of dependencies changed more recently than current target
# $@ name of current target
# $< name of current dependency
# $* name of current dependency without extension

################################################################
# Config

VPATH = ./:../:vimclair_basic
MAKEFLAGS = --no-print-directory

.PHONY: all
all: tar.gz zip

################################################################
# install

# XXX TODO

################################################################
# tar.gz

PACKED_FILES = \
	vimclair_basic/LICENSE.txt \
	vimclair_basic/README.md \
	vimclair_basic/doc/en.program.vimclair_basic.html \
	vimclair_basic/vbas2tap.sh \
	vimclair_basic/vim/ftdetect/vimclairbasic.vim \
	vimclair_basic/vim/ftplugin/vimclairbasic.vim \
	vimclair_basic/vim/plugins/vbas2tap.vim \
	vimclair_basic/vim/syntax/vimclairbasic.vim

vimclair_basic/LICENSE.txt:
vimclair_basic/README.md:
vimclair_basic/doc/en.program.vimclair_basic.html:
vimclair_basic/vbas2tap.sh:
vimclair_basic/vim/ftdetect/vimclairbasic.vim:
vimclair_basic/vim/ftplugin/vimclairbasic.vim:
vimclair_basic/vim/plugins/vbas2tap.vim:
vimclair_basic/vim/syntax/vimclairbasic.vim:

.PHONY: tar.gz
tar.gz:
	@make vimclair_basic.tar.gz

vimclair_basic.tar.gz: $(PACKED_FILES)
	tar \
	--create \
	--gzip \
	--file vimclair_basic.tar.gz \
	--dereference \
	--hard-dereference \
	--directory .. \
	$(PACKED_FILES)

################################################################
# zip

.PHONY: zip
zip:
	@make vimclair_basic.zip

vimclair_basic.zip: vimclair_basic.tar.gz
	cd .. && \
	zip -9 vimclair_basic/vimclair_basic.zip \
	$(PACKED_FILES)

