" sinbasic2tap.vim

" SinBasic2tap
" Copyright (C) 2014 Marcos Cruz (programandala.net)

" License: XXX TODO

" This file is part of SinBasic:
" http://programandala.net/en.program.sinbasic.html

" This program, written in Vim, converts SinBasic source code
" into an actual Sinclair BASIC program stored in a TAP file.

" ------------------------------
" History

" 2014-07-26: Started with the code of SinBasic2BB
" (http://programandala.net/es.programa.bbim).
"

" ------------------------------
" TODO


" ----------------------------------------------

function! SinBasicClean()

  " Clean off all SinBasic stuff.

  silent! %s/^\s*#.*$//e " Remove the metacomments
  silent! %s/\s*\/\/.*$//e " Remove the // line comments
  silent %s,^\s*\/\*\_.\{-}\*\/,,e " Remove the /* */ block comments

  silent! %s/^\s*\d\+\s*$//e " Remove lines with the line number only
  " 2012-01-29 xxx old:
  "silent! %s/^\s\+//e " Remove empty lines
  "silent! %s/\n\n\+/\r/eg " Remove empty lines
  " 2012-01-29 xxx new, untested:
  silent! %s/^\n//e " Remove empty lines

  silent %s,^\s*\n,,ge " Remove the empty lines

  silent! %s/^\s*//eg " Remove main indentation
  silent! %s/\s\+$//eg " Remove ending blanks

  silent! %s/\\\s*\n//e " Join the splitted lines
  silent! %s/^\(\d\+\)\s\+/\1/e " Remove the space after the line number

  echo 'Source code cleaned.'

endfunction

" ----------------------------------------------
" Metacommands

function! SinBasicVim()

  " Execute all #vim metacommands.
  " Syntax:
  " #vim Any-Vim-Ex-Command

  call cursor(1,1) " Go to the top of the file.
  let l:vimCommands=0 " Counter
  while search('^\s*#vim\s','Wc')
    let l:vimCommands += 1
    let l:vimCommandLine = line('.')
    let l:vimCommand=matchstr(getline(l:vimCommandLine),'\S\+.*',4)
    execute 'silent! '.l:vimCommand
    call cursor(l:vimCommandLine,1) " Return to the command line.
    call setline('.','') " Blank the line.
  endwhile

  if l:vimCommands==0
    echo 'No Vim command found.'
  elseif l:vimCommands==1
    echo 'One Vim command executed.'
  else
    echo l:vimCommands 'Vim commands executed.'
  endif

endfunction

function! SinBasicInclude()

  " Execute all #include commands in the source.
  " Syntax:
  " #include file-name
  " Warning: nested including is possible, but no recursion check is made!

  call cursor(1,1) " Go to the top of the file.
  let l:includedFiles=0 " Counter
  while search('^\s*#include\s','Wc')
    let l:includedFiles += 1
    let l:fileName=matchstr(getline('.'),'\S\+.*',8)
    call setline('.','') " Blank the line.
    " ----------- xxx debug check
    "echo '#include ' l:fileName
    "echo 'getcwd()=' getcwd()
    "echo 'Modifications:'
    "echo ':~' fnamemodify(l:fileName,':~')
    "echo ':p' fnamemodify(l:fileName,':p')
    " -----------
    execute "silent! r ".l:fileName
  endwhile

  if l:includedFiles==0
    echo 'No file included.'
  elseif l:includedFiles==1
    echo 'One file included.'
  else
    echo l:includedFiles 'files included.'
  endif

endfunction

" ----------------------------------------------
" Labels

function! SinBasicGetFirstLine()

  " Store into s:firstLine the first line number
  " to be used by the final Beta BASIC program
  " (old versions of SinBasicport occupied lines 1-9).
  " The command #firstline can be used to set
  " the desired line number. Only the first occurence
  " of #firstline will be used; it can be anywhere
  " in the source but always at the start of a line
  " (with optional indentation).

  let s:firstLine=1 " default value
  
  call cursor(1,1) " Go to the top of the file.
  if search('^\s*#firstline\s\+[0-9]\+\>','Wc')
    " Store the number into register 'l':
    normal ww"lyw
    " And then into the variable:
    let s:firstLine=getreg('l',1)
  endif
  echo 'First line number: '.s:firstLine

endfunction

function! SinBasicLabels()

  " Join lonely labels to the next line:
  silent %substitute,^\s*\(label\s\+\)\?\(@[0-9a-zA-Z_]\+\)\s*:\?\s*\n,\2:,ei

  " Empty dictionary to store the line numbers of the labels; the labels will be used as keys:
  let l:lineNumber={}
  call cursor(1,1) " Go to the top of the file.
  " Store every label in the l:lineNumber dictionary:
  while search('^\s*\(label\s\+\)\?@[0-9a-zA-Z_]\+\>','W')
    " Store the label into register 'l':
    normal "l2yw
    " xxx debug check
    "echo 'Raw label found: <' . getreg('l',1) . '>'
    " If 'label' is present, go to the next word and repeat:
    if tolower(getreg('l',1))=='label @'
      normal w"l2yw
      " xxx debug check
      "echo 'Actual raw label found: <' . getreg('l',1) . '>'
    endif
    " Remove possible ending spaces:
    let l:label=tolower(substitute(getreg('l',1),' ','','g'))
    " xxx debug check
    "echo 'Clean label: <' . l:label . '>'
    " Use the label as the key to store the line number:
    let l:lineNumber[l:label]=line('.')+s:firstLine-1
    " Go to the next word:
    normal w
  endwhile
  
  " xxx debug check
  "echo l:lineNumber

  " Remove all labels:
  silent! %substitute/^\s*\(label\s\+\)\?@[0-9a-zA-Z_]\+\s*:\?\s*//ei

  " Substitute every label reference with its line number:
  for l:label in keys(l:lineNumber)
    call cursor(1,1) " Go to the top of the file.
    " Do the subtitution:
    while search(l:label,'Wc')
      " xxx debug check
      "echo l:label "label reference found"
      execute "silent! substitute/".l:label."\\>/".l:lineNumber[l:label]."/ei"
    endwhile
  endfor

  echo 'Labels translated.'

endfunction

" ----------------------------------------------
" Renum

function! SinBasicRenum()

  " Call the the nl program (part of the Debian coreutils package):
  execute "silent! %!nl --body-numbering=t --number-format=rn --number-width=5 --number-separator=' ' --starting-line-number=".s:firstLine." --line-increment=1"

  " In older versions of coreutils,
  " -v sets the first line number, and -i sets the line increment.
  " (the long option for -v doesn't work, though the manual mentions it).
  " Modern versions of nl uses the clearer options
  " --first-line and --line-increment, see:
  " http://www.gnu.org/software/coreutils/manual/coreutils.html#nl-invocation

  " Remove spaces before line numbers
  " (nl has no option to remove them):
  silent! %substitute/^\s*//e

  " Remove line numbers from import-time commands
  silent! %substitute/^[0-9]\{1,4}\s://e

  echo 'Line numbers added.'

endfunction

" ----------------------------------------------
" Token conversion

function! SinBasicTokens()
  silent! %s@\<gosub\>@go sub@ge
  silent! %s@\<goto\>@go to@ge
endfunction

" ----------------------------------------------
" Character translation

function! SinBasicGraphs()

  " Translate graphics from BASin format to BAS2TAP format.
  
  " Block graphics (chars 128-143)

  " XXX TODO
  silent! %s@\\  @\=nr2char(128)@ge
  silent! %s@\\ '@\=nr2char(129)@ge
  silent! %s@\\' @\=nr2char(130)@ge
  silent! %s@\\''@\=nr2char(131)@ge
  silent! %s@\\ \.@\=nr2char(132)@ge
  silent! %s@\\ :@\=nr2char(133)@ge
  silent! %s@\\'\.@\=nr2char(134)@ge
  silent! %s@\\':@\=nr2char(135)@ge
  silent! %s@\\\. @\=nr2char(136)@ge
  silent! %s@\\\.'@\=nr2char(137)@ge
  silent! %s@\\: @\=nr2char(138)@ge
  silent! %s@\\:'@\=nr2char(139)@ge
  silent! %s@\\\.\.@\=nr2char(140)@ge
  silent! %s@\\\.:@\=nr2char(141)@ge
  silent! %s@\\:\.@\=nr2char(142)@ge
  silent! %s@\\::@\=nr2char(143)@ge

  " UDG (chars 144-164)

  silent! %s@\\[Aa]@{A}@ge
  silent! %s@\\[Bb]@{B}@ge
  silent! %s@\\[Cc]@{C}@ge
  silent! %s@\\[Dd]@{D}@ge
  silent! %s@\\[Ee]@{E}@ge
  silent! %s@\\[Ff]@{F}@ge
  silent! %s@\\[Gg]@{G}@ge
  silent! %s@\\[Hh]@{H}@ge
  silent! %s@\\[Ii]@{I}@ge
  silent! %s@\\[Jj]@{J}@ge
  silent! %s@\\[Kk]@{K}@ge
  silent! %s@\\[Ll]@{L}@ge
  silent! %s@\\[Mm]@{M}@ge
  silent! %s@\\[Nn]@{N}@ge
  silent! %s@\\[Oo]@{O}@ge
  silent! %s@\\[Pp]@{P}@ge
  silent! %s@\\[Qq]@{Q}@ge
  silent! %s@\\[Rr]@{R}@ge
  silent! %s@\\[Ss]@{S}@ge
  silent! %s@\\[Tt]@{T}@ge
  silent! %s@\\[Uu]@{U}@ge

endfunction

function! SinBasicChars()

  let l:ignoreCaseBackup=&ignorecase
  set noignorecase

  call SinBasicGraphs()

  " Embedded ASCII codes (BASin format):
  silent! %s/\\#\(\d\+\)/\=nr2char(submatch(1))/g

  echo 'Special chars translated.'

  if l:ignoreCaseBackup
    set ignorecase
  else
    set noignorecase
  endif

endfunction

" ----------------------------------------------
" BAS file

function! SinBasicBasfile()

  " Create a copy of the current SinBasic file
  " with the ".bas" extension added
  " and open it for editing.

  silent update " Write the current SinBasic file if needed
  split " Split the window
  silent write! %.bas " Save a copy with the BB extension added
  silent edit %.bas " Open it for editing
  set fileencoding=latin1

  echo 'BAS file created.'

endfunction

" ----------------------------------------------
" Main

function! SinBasic2tap()

  set shortmess=at

  call SinBasicGetFirstLine()

  let s:ignoreCaseBackup=&ignorecase
  set ignorecase

  call SinBasicBasfile()

  call SinBasicInclude()
  call SinBasicVim()
  call SinBasicClean()
  call SinBasicLabels()
  call SinBasicRenum()
  call SinBasicChars()
  call SinBasicTokens()

  silent w
  silent bw 
  echo 'BAS file saved and closed.'

  if s:ignoreCaseBackup
    set ignorecase
  else
    set noignorecase
  endif

  echo 'Done!'

endfunction

" Shortkey ',sb' in normal mode
" to create a Beta BASIC file:
nmap <silent> ,sb :call SinBasic2tap()<CR>

echo "SinBasic2tap loaded."
echo "Activate it with the keys ',sb' (comma, S and B), in normal mode, on your SinBasic source."
