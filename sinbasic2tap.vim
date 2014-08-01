" sinbasic2tap.vim

" SinBasic2tap
" Version A-00-201408011759

" Latest improvement: do-loop, do-loop until, do-loop while.

" Copyright (C) 2014 Marcos Cruz (programandala.net)

" License: XXX TODO

" This file is part of SinBasic:
" http://programandala.net/en.program.sinbasic.html

" This program, written in Vim, converts SinBasic source code to an actual
" Sinclair BASIC program stored in a TAP file.

" ----------------------------------------------
" XXX TODO

" A lot.
" Use single quotes when possible

" ----------------------------------------------
" History

" 2014-07-26: Started with the code of SinBasic2BB
" (http://programandala.net/es.programa.bbim).
" New: 'do...loop', 'do...loop until', 'do...loop while'.

" 2014-07-27:
" New: First draft of 'do until...loop', 'do while...loop' and nested
" loops.

" 2014-07-31:
" Fix: Some local variables changed to script variables.

" 2014-08-01:
" Fix: All seven combinations of 'do...loop' work fine.
" Fix: Nested loops work fine.
" New: 'exit do' implemented.

" ----------------------------------------------
" TODO

" 2014-08-01: Improve: The parens used by 'NOT' to enclose the WHILE or UNTIL
" expression may be ommited in certain cases.

function! SinBasicClean()

  " Clean the source code.

  silent! %s/^\s*#.*$//e " Remove the metacomments
  silent! %s/\s*\/\/.*$//e " Remove the // line comments
  silent %s,^\s*\/\*\_.\{-}\*\/,,e " Remove the /* */ block comments
  silent! %s/^\s\+//e " Remove indentation
  silent! %s/\s\+$//e " Remove trailing spaces
  silent! %s/^\n//e " Remove the empty lines
  silent! %s/\\\n//e " Join the splitted lines

  echo 'Source code cleaned.'

endfunction

" ----------------------------------------------
" Control structures

function! SinBasicControlStructures()

  call SinBasicDoLoop()

endfunction

function! SinBasicDoLoop()

  " Convert all DO-LOOP structures.

  " The SinBasic DO-LOOP structures are copied from Andy Wright's Beta BASIC,
  " SAM BASIC and MasterBASIC: they allow UNTIL and WHILE in any combination
  " (there are seven possible combinations).

  let s:doStatement=''

  echo "--- About to search a DO!"
  call cursor(1,1)
  while search('^do\(\s\+\(until\|while\)\s\+.\+\)\?$','Wc')
    " first DO found
    echo "--- DO found!"
    let s:doLineNumber=line('.') " line number of the DO statement
    call SinBasicDo()
    let l:unclosedLoops=1 " counter
    while search('^\(do\|loop\)\>','W')
      " DO or LOOP found
      echo '--- DO or LOOP found'
      echo 'line: '.getline('.')
      if strpart(getline('.'),0,2)=='do'
        " DO
        let l:unclosedLoops=l:unclosedLoops+1
      else
        " LOOP
        let l:unclosedLoops=l:unclosedLoops-1
        if l:unclosedLoops==0
          call SinBasicLoop()
          break
        endif
      endif
    endwhile
    if l:unclosedLoops
      echo 'Error: DO without LOOP at line '.doLineNumber
    endif
    call cursor(s:doLineNumber,'$')
  endwhile

  call SinBasicExitDo()

endfunction

function! SinBasicDo()

  " Open a DO-LOOP.

  " The loop start can be DO, DO UNTIL or DO WHILE.  If it's just DO, a label
  " is enough.  If it's DO UNTIL or DO WHILE, a conditional jump has to be
  " inserted, but the destination line is unknown until the correspondant LOOP
  " is found.  Therefore the code in stored into s:doStatement in order to
  " create it later (SinBasicLoop() does it), with the added line number.

  " Save the original line: 
  let l:doLine=getline('.')

  " Put a label instead:
  call setline('.','@do'.s:doLineNumber)

  " Check the kind of DO and calculate the proper statement:
  " Note: '\c' at the start of the patterns ignores case.
  if match(l:doLine,'\c^do\s\+while\>')>-1
    let l:conditionPos=matchend(l:doLine,'^do\s\+while\s\+')
    let l:condition=strpart(l:doLine,l:conditionPos)
    let s:doStatement='if not ('.l:condition.') then goto '
  elseif match(l:doLine,'\c^do\s\+until\>')>-1
    let l:conditionPos=matchend(l:doLine,'^do\s\+until\s\+')
    let l:condition=strpart(l:doLine,l:conditionPos)
    let s:doStatement='if '.l:condition.' then goto '
  elseif match(l:doLine,'\c^do$')>-1
    let s:doStatement=''
  else
    echo 'Error: DO bad syntax at line '.line('.').'.'
  endif

endfunction

function! SinBasicLoop()

  " Close a DO-LOOP.

  let l:loopLine=getline('.')
  let l:jump='goto @do'.s:doLineNumber
  echo '------ Right LOOP: '.l:loopLine
  if match(l:loopLine,'^loop\s\+while\>')>-1
    execute 'substitute,^loop\s\+while\s\+\(.\+\)$,if \1 then '.l:jump.',i'
  elseif match(l:loopLine,'^loop\s\+until\>')>-1
    execute 'substitute,^loop\s\+until\s\+\(.\+\)$,if not (\1) then '.l:jump.',i'
  elseif match(l:loopLine,'^loop$')>-1
    execute 'substitute,^loop$,'.l:jump.',i'
  else
    echo 'Error: LOOP bad syntax at line '.line('.').'.'
  endif

  " Create a label after the end of the loop
  " (it may be needed by DO WHILE, DO UNTIL or EXIT DO):
  let l:loopExitLabel='@loopExit'.s:doLineNumber
  call append('.',l:loopExitLabel)

  " Finish the DO if necessary:
  if s:doStatement!=''
    " Complete and create the jump to it:
    call append(s:doLineNumber,s:doStatement.l:loopExitLabel)
    let s:doStatement=''
  endif

endfunction

function! SinBasicExitDo()

  " Convert EXIT DO.

  let s:doStatement=''

  echo "--- About to search a EXIT DO!"
  call cursor(1,1)
  while search('\<exit do$','Wc')
    echo "--- EXIT DO found!"
    let l:exitDoLineNumber=line('.')
    if search('^@loopExit\d\+$','W')
      let l:exitLabel=getline('.')
      call cursor(l:exitDoLineNumber,'^')
      execute 'silent! %substitute,\<exit do\>,goto '.l:exitLabel.',ei'
    else
      echo 'Error: EXIT DO without LOOP at line '.doLineNumber
    endif
  endwhile
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
    " ----------- XXX debug check
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

  let l:ignoreCaseBackup=&ignorecase
  set ignorecase

  " Join every lonely label to its following line
  " (unless its following line has another label definition):
  silent %substitute,^\(@[0-9a-zA-Z_]\+\)\s*:\?\n\([^@]\),\1:\2,ei

  " Create an empty dictionary to store the line numbers of the labels;
  " the labels will be used as keys:
  let l:lineNumber={}
  
  " Go to the top of the file:
  call cursor(1,1)

  " Search for label definitions and store them into the dictionary:
  while search('^@[0-9a-zA-Z_]\+\>','W')
    " Store the found label into register 'l':
    normal "l2yw
    " XXX debug check
"    echo 'Raw label found: <' . getreg('l',1) . '>'
"    let l:label=tolower(getreg('l',1))
    let l:label=getreg('l',1)
    " XXX debug check
"    echo 'Clean label: <' . l:label . '>'
    " Use the label as the key to its line number:
    let l:lineNumber[l:label]=line('.')+s:firstLine-1
    " Go to the next word:
    normal w
  endwhile

  " Remove all label definitions:
  silent! %substitute/^@[0-9a-zA-Z_]\+\s*:\?\s*//ei

  " Substitute every label reference with its line number:
  for l:label in keys(l:lineNumber)
    "echo "About to search for label " l:label
    call cursor(1,1) " Go to the top of the file.
    " Do the subtitution: 
    while search(l:label.'\>','Wc')
      " XXX debug check
"      echo l:label "label reference found"
"      echo "About to translate it to " l:lineNumber[l:label]
      "execute 'silent! substitute/\<'.l:label.'\>/'.l:lineNumber[l:label].'/ei'
      execute 'substitute/'.l:label.'\>/'.l:lineNumber[l:label].'/i'
    endwhile
  endfor

  if l:ignoreCaseBackup
    set ignorecase
  else
    set noignorecase
  endif

  echo 'Labels translated.'

endfunction

" ----------------------------------------------
" Renum

function! SinBasicRenum()

  " Call the nl program (part of the Debian coreutils package):
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
  silent! %s@\<deffn\>@def fn@ge
endfunction

" ----------------------------------------------
" Character translation

function! SinBasicGraphs()

  " Translate graphics from BASin format to BAS2TAP format.
  
  " Block graphics (chars 128-143)

  " XXX TODO
  silent! %s@\\  @{80}@ge
  silent! %s@\\ '@{81}@ge
  silent! %s@\\' @{82}@ge
  silent! %s@\\''@{83}@ge
  silent! %s@\\ \.@{84}@ge
  silent! %s@\\ :@{85}@ge
  silent! %s@\\'\.@{86}@ge
  silent! %s@\\':@{87}@ge
  silent! %s@\\\. @{88}@ge
  silent! %s@\\\.'@{89}@ge
  silent! %s@\\: @{8A}@ge
  silent! %s@\\:'@{8B}@ge
  silent! %s@\\\.\.@{8C}@ge
  silent! %s@\\\.:@{8D}@ge
  silent! %s@\\:\.@{8E}@ge
  silent! %s@\\::@{8F}@ge

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

  " XXX TODO finish:

  silent! %s@\\{vi}@{INVERSE 1}@ge
  silent! %s@\\{vn}@{INVERSE 0}@ge

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
  call SinBasicControlStructures()
" XXX TMP
"  call SinBasicLabels()
"  call SinBasicRenum()
"  call SinBasicChars()
"  call SinBasicTokens()

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

" vim:tw=78:ts=2:et:
