" ~/.vim/ftplugin/vimclairbasic.vim

" Vim filetype plugin for Vimclair BASIC

" By Marcos Cruz (programandala.net)

" This file is part of Vimclair BASIC
" http://programandala.net/en.program.vimclair_basic.html

" Change history
"
" 2014-07-25: First version, adapted from <~/.vim/ftplugin/bbim.vim>.
" 2014-08-05: Renamed and updated.
" 2014-08-09: New: 'comments' and 'formatoptions'.
" 2014-08-12: Change: expandtab.
" 2014-12-15: Change: The 'nmap ,vb' and its help text are moved here from the
" converter.
" 2015-02-10: Updated.
" 2015-03-19: The code is executed with 'runtime', not with 'source'; this way
" it can be stored in any directory of the 'runtimepath'.

setlocal tabstop=2
setlocal softtabstop=0
setlocal shiftwidth=2
setlocal expandtab
setlocal foldmethod=marker
setlocal textwidth=70
setlocal comments=b:#,://

setlocal formatoptions=cqorj

" Note: The "j" flag in 'formatoptions' removes a comment leader when joining
" lines.  See ':help fo-table' for details.

" Used by the Vim-Commentary plugin:
"setlocal commentstring=//\ %s
" '#' is more convenient, because it's used for commenting out, not for actual
" comments:
setlocal commentstring=#\ %s

" The key sequence '.tap' in normal mode
" runs the vbas2tap converter:

runtime vbas2tap.vim
nmap <silent> .tap :call Vbas2tap()<CR>

