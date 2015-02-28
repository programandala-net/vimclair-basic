" vbas2tap.vim

" vbas2tap
let s:version='A-07-20150228'

" This file is part of Vimclair BASIC
" http://programandala.net/en.program.vimclair_basic.html

" This file converts a Vimclair BASIC source to a TAP file

" Copyright (C) 2014,2015 Marcos Cruz (programandala.net)

" vbas2tap is free software; you can redistribute it and/or
" modify it under the terms of the GNU General Public License as
" published by the Free Software Foundation; either version 3 of
" the License, or (at your option) any later version.
"
" vbas2tap is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public
" License along with vbas2tap; if not, see
" <http://gnu.org/licenses>.

" ----------------------------------------------
" Description

" vbas2tap converts a Vimclair BASIC source file
" to Sinclair BASIC in a TAP file.

" More details in the <README.adoc> file and
" <http://programandala.net/en.program.vimclair_basic.html>.

" ----------------------------------------------
" Requirements

" This program needs either BAS2TAP or zmakebas:

" BAS2TAP (by Martijn van der Heide of ThunderWare Research
" Center)
"
" At the time of writing, its latest version (v2.4, release
" 2005-07-24), can be obtained from the utilities section of
" World of Spectrum (http://www.worldofspectrum.org/); or you
" can try <ftp://ftp.worldofspectrum.org/pub/sinclair/tools/pc>.
"
" This is how I compiled and installed BAS2TAP on Debian:
"
"    gcc -Wall -O2 bas2tap.c -o bas2tap -lm
"    strip bas2tap
"    sudo mv bas2tap /usr/local/bin/

" zmakebas (by Russel Marks)
"
" At the time of writing, its latest version (1.2, release 2004)
" is a package of Debian, Raspbian, Ubuntu and probably other
" distros.


" ----------------------------------------------

function! VimclairClean()

  " Clean the source code.

  " Save the #vim commands XXX OLD
"  let l:mark='vimcommand'.localtime()
"  execute 'silent! %s/^\s*#vim\>/'.l:mark.'/ei'

  " Remove the metacomments
  silent! %s/^\s*#\(\s.*\)\?$//e

  " Restore the #vim commands XXX OLD
"  execute 'silent! %s/^'.l:mark.'/#vim/e'

  silent! %s/\s*\/\/.*$//e " Remove the // line comments
  silent! %s,^\s*\/\*\_.\{-}\*\/,,e " Remove the /* */ block comments
  silent! %s/^\s\+//e " Remove indentation
  silent! %s/\s\+$//e " Remove trailing spaces
  silent! %s/^\n//e " Remove the empty lines
  silent! %s/\\\n//e " Join the splitted lines

  echo 'Source code cleaned'

  call VimclairSaveStep('clean')
  
endfunction

" ----------------------------------------------
" Control structures

function! VimclairControlStructures()

  call VimclairDoLoop()
  call VimclairExitFor()
  call VimclairIfEndif()
  call VimclairProcedures()

endfunction

function! VimclairDoLoop()

  " Convert all 'DO LOOP' structures.

  " The Vimclair BASIC 'DO LOOP' structures are copied from Andy Wright's Beta
  " BASIC, SAM BASIC and MasterBASIC: they allow 'UNTIL' and 'WHILE' in any
  " combination (there are nine possible combinations).

  let s:doStatement=''

  "echo '  XXX About to search for a DO!'
  call cursor(1,1)
  while search('^do\(\s\+\(until\|while\)\s\+.\+\)\?$','Wc')
    " first DO found
    "echo '  XXX DO found!'
    let s:doLineNumber=line('.') " line number of the DO statement
    call VimclairDo()
    let l:unclosedLoops=1 " counter
    while search('^\(do\|loop\)\>','W')
      " DO or LOOP found
      "echo '  XXX DO or LOOP found'
      "echo '  XXX line: '.getline('.')
      if strpart(getline('.'),0,2)=='do'
        " DO
        let l:unclosedLoops=l:unclosedLoops+1
      else
        " LOOP
        let l:unclosedLoops=l:unclosedLoops-1
        if l:unclosedLoops==0
          call VimclairLoop()
          break
        endif
      endif
    endwhile
    if l:unclosedLoops
      echoerr 'DO without LOOP at line '.s:doLineNumber
    endif
    call cursor(s:doLineNumber,'$')
  endwhile

  call VimclairExitDo()
  
  call VimclairSaveStep('do_loop')

endfunction

function! VimclairDo()

  " Open a 'DO LOOP' structure.

  " Syntax:

  " 'DO' and 'LOOP' (with optional 'UNTIL' or 'WHILE' condition) must be the
  " only statements on their lines.

  " How this works:

  " The loop start can be 'DO', 'DO UNTIL' or 'DO WHILE'.  If it's just 'DO',
  " a label is enough.  If it's 'DO UNTIL' or 'DO WHILE', a conditional jump
  " has to be inserted, but the destination line is unknown until the
  " correspondent 'LOOP' is found.  Therefore the code is stored into
  " 's:doStatement' in order to create it later ('VimclairLoop()' does it),
  " with the line number added.

  " Save the original line: 
  let l:doLine=getline('.')

  " Put a label instead:
  call setline('.','@do'.s:doLineNumber)

  " Check the kind of 'DO' and calculate the proper statement:
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
    echoerr 'DO bad syntax at line '.line('.')
  endif

endfunction

function! VimclairLoop()

  " Close a 'DO LOOP' structure.

  let l:loopLine=getline('.')
  let l:jump='goto @do'.s:doLineNumber
  "echo '  XXX--- Right LOOP: '.l:loopLine
  if match(l:loopLine,'^loop\s\+while\>')>-1
    execute 'substitute,^loop\s\+while\s\+\(.\+\)$,if \1 then '.l:jump.',i'
  elseif match(l:loopLine,'^loop\s\+until\>')>-1
    execute 'substitute,^loop\s\+until\s\+\(.\+\)$,if not (\1) then '.l:jump.',i'
  elseif match(l:loopLine,'^loop$')>-1
    execute 'substitute,^loop$,'.l:jump.',i'
  else
    echoerr 'LOOP bad syntax at line '.line('.')
  endif

  " Create a label after the end of the loop
  " (it may be needed by 'DO WHILE', 'DO UNTIL' or 'EXIT DO'):
  let l:loopExitLabel='@loopExit'.s:doLineNumber
  call append(line('.'),l:loopExitLabel)

  " Finish the 'DO' if necessary:
  if s:doStatement!=''
    " Complete and create the jump to the 'DO':
    call append(s:doLineNumber,s:doStatement.l:loopExitLabel)
    let s:doStatement=''
  endif

endfunction

function! VimclairExitDo()

  " Convert 'EXIT DO'.

  " 'EXIT DO' must be at the end of a line.
  " The form 'EXITDO' is allowed.

  let s:doStatement=''

  "echo '  XXX About to search for an EXIT DO!'
  call cursor(1,1)
  while search('\<exit\s\?do$','Wc')
    "echo '  XXX EXIT DO found!'
    let l:exitDoLineNumber=line('.')
    if search('^@loopExit\d\+$','W')
      let l:exitLabel=getline('.')
      call cursor(l:exitDoLineNumber,'^')
      execute 'silent! substitute,\<exit\s\?do\>,goto '.l:exitLabel.',ei'
    else
      echoerr 'EXIT DO without LOOP at line '.exitDoLineNumber
    endif
  endwhile

endfunction

function! VimclairExitFor()

  " Convert 'EXIT FOR'.

  " 'EXIT FOR' and its correspondent 'NEXT' must be at the end of the line.
  " The form 'EXITFOR' is allowed.

  let s:doStatement=''

  call cursor(1,1)
  while search('\<exit\s\?for$','Wc')
    "echo '  XXX EXIT FOR found at line '.line('.').': '.getline('.')
    let l:exitForLineNumber=line('.')
    if search('\<next [a-z]\>','W')
      "echo '  XXX NEXT found at line '.line('.').': '.getline('.')
      let l:exitLabel='@forExit'.line('.')
      call append(line('.'),l:exitLabel)
      call cursor(l:exitForLineNumber,'^')
      execute 'silent! substitute,\<exit\s\?for\>,goto '.l:exitLabel.',ei'
    else
      echoerr 'EXIT FOR without NEXT at line '.exitForLineNumber
    endif
  endwhile
  
  call VimclairSaveStep('exit_for')

endfunction

function! VimclairIfEndif()

  " Convert all 'IF ENDIF' structures.

  " XXX TODO finish

  " The Vimclair BASIC's 'IF ENDIF' structures are inspired by Andy Wright's
  " Beta BASIC, SAM BASIC and MasterBASIC, but they are not identical.

  " Syntax:

  " Short 'IF' structures are the same than Sinclair BASIC's.

  "   IF condition THEN action

  " Of course they can be splitted into any number of text lines:

  "   IF condition THEN \
  "     action

  " As usual, the splitting format does not affect the parsing, as long as the
  " required spaces are preserved before the splitting points:
  
  "   IF \
  "     condition \
  "   THEN \
  "     action
  
  " Long 'IF' structures must have 'THEN' after every condition; 'IF' and
  " 'ELSE IF' must be at the start of the line; 'ELSEIF' is allowed; 'ELSE'
  " must be on its own line:
  
  "   IF condition1 THEN
  "     code1
  "   ELSE IF condition2 THEN
  "     code2
  "   ELSE 
  "     code3
  "   END IF

  "echo '  XXX About to search for a long IF!'
  call cursor(1,1)
  while search('^if .\+ then$','Wc')

    " Main long 'IF' found
"    echo 'XXX IF found'
"    echo '<'.getline('.').'>'
    let l:ifLineNumber=line('.')
    let l:endifLabel='@endif'.l:ifLineNumber
    let l:conditionLineNumber=line('.')
    let l:condition=substitute(getline('.'), '^if\s\+\(.\{-}\)\s\+then$', '\1', '')
    let l:unclosedConditionals=1 " counter
    let l:elseLineNumber=0 " used also as a flag

    while search('^\(\(else\s\?\)\?if\s\+.\+\s\+then\|else\|end\s\?if\)$','W')
      " Nested long 'IF', 'ELSE IF', 'ELSE' or 'ENDIF' found
"      echo '  XXX IF, ELSE IF, ELSE or ENDIF found'
"      echo '  <'.getline('.').'>'
      if strpart(getline('.'),0,2)=='if'
        " Nested long IF
        let l:unclosedConditionals=l:unclosedConditionals+1
      elseif match(getline('.'),'else\s\?if')==0
        " ELSE IF
"        echo '  XXX ELSE IF found'
"        echo '  <'.getline('.').'>'
        if l:unclosedConditionals==1 " current IF structure?
          if l:elseLineNumber " there was a previous ELSE?
            echoerr 'ELSE IF after ELSE at line '.line('.')
            break
          else
            call append(line('.')-1,'goto '.l:endifLabel)
            " Make the previous condition jump here when false:
            let l:elseIfLabel='@elseIf'.l:ifLineNumber.'_'.line('.')
            let l:newIf='if '.VimclairNot(l:condition).' then goto '.l:elseIfLabel
            call setline(l:conditionLineNumber,l:newIf)
            call append(line('.')-1,l:elseIfLabel)
            " Keep the current condition:
            let l:conditionLineNumber=line('.')
            let l:condition=substitute(getline('.'), '^else\s\?if\s*\(.\{-}\)\s*then$', '\1', '')
            " 
          endif
        endif
      elseif getline('.')=='else'
        " ELSE
"        echo '  XXX ELSE found'
"        echo '  <'.getline('.').'>'
        if l:unclosedConditionals==1 " current IF structure?
          if l:elseLineNumber " there was a previous ELSE?
            echoerr 'Second ELSE at line '.line('.')
            break
          else
            call append(line('.')-1,'goto '.l:endifLabel)
            let l:elseLineNumber=line('.')
            " Make the previous condition jump here when false:
            let l:elseLabel='@else'.l:ifLineNumber
            let l:newIf='if '.VimclairNot(l:condition).' then goto '.l:elseLabel
            call setline(l:conditionLineNumber,l:newIf)
            call setline('.',l:elseLabel)
            " Keep the current condition:
            let l:conditionLineNumber=line('.') " XXX needed?
            let l:condition=''
          endif
        endif
      else
        " ENDIF
"        echo '  XXX ENDIF found'
"        echo '  <'.getline('.').'>'
        let l:unclosedConditionals=l:unclosedConditionals-1
        if l:unclosedConditionals==0 " current IF structure?
          call setline('.',l:endifLabel)
          if len(l:condition) " is there an unresolved condition?
            let l:newIf='if '.VimclairNot(l:condition).' then goto '.l:endifLabel
            call setline(l:conditionLineNumber,l:newIf)
          endif
          break
        endif
      endif
    endwhile

    if l:unclosedConditionals
      echoerr 'IF without ENDIF at line '.l:ifLineNumber
    endif

    call cursor(l:ifLineNumber,'$')
  endwhile

  call VimclairSaveStep('if_endif')

endfunction

function! VimclairNot(expression)

  " Return the opposite of the given expression.
  " If the expression already has a 'NOT' at the start, just remove it;
  " otherwise, add a 'NOT' to it.

  let l:expression=a:expression
  let l:expression=substitute(l:expression,'^\s*\(.\{-}\)\s*$','\1','')
  " XXX Somehow, '\>' does not work in the following pattern, so
  " '[ (]' is used instead:
  if match(l:expression,'^not[ (]')==0
    let l:expression=substitute(l:expression,'^not\s*\(.\{-}\)$','\1','')
    let l:expression=VimclairWithoutParens(l:expression)
  else
    let l:expression='not('.l:expression.')'
  endif
  return l:expression

endfunction

function! VimclairWithoutParens(expression)

  " Remove the outside parens from the given expression.

  let l:expression=a:expression
  " XXX TODO improve the expression (use just one match() instead, if possible)
  while match(l:expression,'(')==0 && strpart(l:expression,len(l:expression)-1)==')'
    let l:expression=substitute(l:expression,'^(\s*\(\{-}\)\s*)$','\1','')
  endwhile
  return l:expression

endfunction

function! VimclairProcedures()

  " Convert DEF PROC, END PROC and EXIT PROC.

  " Syntax:

  " 'DEF PROC' and 'END PROC' must be the only statements of the line.  The
  " space is optional: 'DEFPROC', 'ENDPROC' and 'EXITPROC' are valid.  'EXIT
  " PROC' must be at the end of the line.  'CALL' must be used to call a
  " procedure, but it can be changed to anything (e.g. 'PROC', or even an
  " empty string) with '#procedureCall'.

  " Description:

  " Procedures are simulated with ordinary routines. No parameters are allowed
  " yet. Procedures with parameters can be simulated with the #vim directive.

  let s:doStatement=''

  call cursor(1,1)
  while search('^def\s\?proc\>','Wc')
    "echo '  XXX DEF PROC found at line '.line('.').': '.getline('.')
    let l:procLineNumber=line('.')
    let l:procLine=getline('.')
    let l:procNamePos=matchend(l:procLine,'^def\s\?proc\s\+')
    let l:procName=strpart(l:procLine,l:procNamePos)
    if len(l:procName)==0
      echoerr 'DEF PROC without name at line '.l:procLineNumber
    else
      " XXX FIXME some procs are not converted
      "echo '  XXX valid proc name: '.l:procName
      let l:procLabel='@proc_'.l:procName
      "echo '  XXX proc label: '.l:procLabel
      call setline(l:procLineNumber,l:procLabel)
      call cursor(l:procLineNumber,'^')
      "echo '  XXX #procedureCall: <'.s:procedureCall.'>'
      if len(s:procedureCall)
        execute 'silent! %substitute,\<'.s:procedureCall.'\s\+'.l:procName.'\>,gosub '.l:procLabel.',gei'
      else
        execute 'silent! %substitute,\<'.l:procName.'\>,gosub '.l:procLabel.',gei'
      endif
    endif
    call cursor(l:procLineNumber,'$')
  endwhile

  silent! %substitute,\<exit\s\?proc$,return,ei
  silent! %substitute,^end\s\?proc$,return,ei

  call VimclairSaveStep('procedures')
  
endfunction

" ----------------------------------------------
" Metacommands

function! VimclairVim()

  " Execute all #vim metacommands.

  " Syntax:
  " #vim Any-Vim-Ex-Command

  call cursor(1,1) " Go to the top of the file.
  let l:vimCommands=0 " Counter
  while search('^\s*#vim\s','Wc')
    let l:vimCommands += 1
    let l:vimCommandLine = line('.')
    let l:vimCommand=matchstr(getline(l:vimCommandLine),'\S\+.*',4)
    " XXX TODO make 'silent' configurable
    " XXX with 'silent', wrong regexp in substitutions are hard to notice!
    execute 'silent! '.l:vimCommand
    " execute l:vimCommand
    call cursor(l:vimCommandLine,1) " Return to the command line.
    call setline('.','') " Blank the line.
  endwhile

  silent! %s/^\n//e " Remove the empty lines

  if l:vimCommands==0
    echo 'No Vim command found'
  elseif l:vimCommands==1
    echo 'One Vim command executed'
  else
    echo l:vimCommands 'Vim commands executed'
  endif

  call VimclairSaveStep('vim_commands')
  
endfunction

function! VimclairInclude()

  " Execute all #include commands in the source.

  " Syntax:
  " #include file-name

  " Warning: nested including is possible, but no recursion check is made!

  call cursor(1,1) " Go to the top of the file.
  let l:includedFiles=0 " Counter
  while search('^\s*#include\s\+','Wc')
    let l:includedFiles += 1
    let l:filename=matchstr(getline('.'),'\S\+.*',8)
    call setline('.','// <<< start of included file '.l:filename) 
    call append('.','// >>> end of included file '.l:filename) 
    let l:filecontent=readfile(s:sourceFileDir.'/'.l:filename)
    call append('.',l:filecontent)
  endwhile

  if l:includedFiles==0
    echo 'No file included'
  elseif l:includedFiles==1
    echo 'One file included'
  else
    echo l:includedFiles 'files included'
  endif

  call VimclairSaveStep('included_files')
  
endfunction

function! VimclairConditionalConversion()

  " Parse and interpret all conditional conversion directives.

  " XXX TODO finish

  " Syntax:
  "
  "   #if[n]def tag
  "     ...
  "   #else
  "     ...
  "   #endif

  " Note: The conditions can not be nested.

  call cursor(1,1)

  let l:unresolvedCondition=0 " flag

  while search('^\s*#if\(n\)\?def\s\+.\+$','Wc')
  
    let l:else=0 " flag

"    echo 'XXX first #if[n]def found'

    while line('.')<line('$') " not at the end of the file?

      let l:currentLine=getline('.')

      if l:currentLine=~'^\s*#ifdef\s\+.\+'
        " #IFDEF
"        echo 'XXX #ifdef found'
        if l:unresolvedCondition
          echoerr '#ifdef structures can not be nested'
          break
        else
          call VimclairIfdef()
          let l:unresolvedCondition=1
        endif
      elseif l:currentLine=~'^\s*#ifndef\s\+.\+'
        " #IFNDEF
"        echo 'XXX #ifndef found ----------------------'
        if l:unresolvedCondition
          echoerr '#ifndef structures can not be nested'
          break
        else
          call VimclairIfdef()
          let l:unresolvedCondition=1
        endif
"      elseif l:currentLine=~'^\s*#elseifdef\s\+.\+'
"        " #ELSEIFDEF
"        call setline('.','')
"        if !l:unresolvedCondition
"          " XXX TODO
"        endif
"      elseif l:currentLine=~'^\s*#elseifndef\s\+.\+'
"        " #ELSEIFNDEF
"        call setline('.','')
"        if !l:unresolvedCondition
"          " XXX TODO
"        endif
      elseif l:currentLine=~'^\s*#else\s*$'
        " #ELSE
"        echo 'XXX #else found'
        if l:else
          echoerr 'More than one #else in the same #if[n]def structure'
          break
        else
          let l:else=1
          call setline('.','')
          let s:keepSource=!s:keepSource
        endif
      elseif l:currentLine=~'^\s*#endif\s*$'
        " #ENDIF
"        echo 'XXX #endif found'
        call setline('.','')
        let l:unresolvedCondition=0
        break
      else
        if l:unresolvedCondition && !s:keepSource
            call setline('.','')
        endif
      endif

      call cursor(line('.')+1,1) " go to the next line

    endwhile

    if l:unresolvedCondition
      echoerr '#IF[N]DEF without #ENDIF at line '.l:ifLineNumber
    endif

  endwhile

  echo 'Conditional conversion done'
  call VimclairSaveStep('conditional_conversion')

endfunction

function! VimclairIfdef()

    let l:ifLineNumber=line('.')
    let l:tagPos=matchend(getline('.'),'^\s*#if\(n\)\?def\s\+')
    let l:tag=Trim(strpart(getline('.'),l:tagPos))
"    echo 'XXX l:tag='.l:tag
    let l:tagMustBeDefined=(getline('.')=~'^\s*#ifdef')
"    echo 'XXX l:tagMustBeDefined='.l:tagMustBeDefined
    let l:tagIsDefined=VimclairDefined(l:tag)
"    echo 'XXX l:tagIsDefined='.l:tagIsDefined
    let s:keepSource=(l:tagMustBeDefined && l:tagIsDefined) || (!l:tagMustBeDefined && !l:tagIsDefined)
"    echo 'XXX s:keepSource='.s:keepSource
    call setline('.','')

endfunction

" ----------------------------------------------
" Config commands

function! VimclairConfig()

  " Search and parse the config commands.  They can be anywhere in the source
  " but always at the start of a line (with optional indentation).

  " #renumLine <line number>
  call VimclairRenumLine()
  " #procedureCall <keyword>
  call VimclairProcedureCall()
  " #runLabel <label>
  call VimclairRunLabel()
  " #define <tag>
  call VimclairDefine()
  " #filename <filename>
  call VimclairZXFilename()
  " #tapmaker <program>
  call VimclairTAPMaker()
  
  call VimclairSaveStep('config_values')

endfunction

function! VimclairRenumLine()

  " Store into 's:renumLine' the first line number to be used by the final
  " Sinclair BASIC program.  The command '#renumLine' can be used to set the
  " desired line number. Only the first occurence of '#renumLine' will be
  " used; it can be anywhere in the source but always at the start of a line
  " (with optional indentation).

  let s:renumLine=1 " default value
  
  call cursor(1,1) " Go to the top of the file.
  if search('^\s*#renumline\>','Wc')
    let l:valuePos=matchend(getline('.'),'^\s*#renumline\s*')
    let s:renumLine=strpart(getline('.'),l:valuePos)
    call setline('.','')
  endif
  " XXX TODO check the number
  echo 'Renum line: '.s:renumLine

endfunction

function! VimclairProcedureCall()

  " Store into 's:procedureCall' the command used in the source by the
  " Sinclair BASIC program.  The command '#procedureCall' can be used to set
  " the desired line number. Only the first occurence of '#procedureCall' will
  " be used; it can be anywhere in the source but always at the start of a
  " line (with optional indentation).

  let s:procedureCall='call' " default value
  
  call cursor(1,1) " Go to the top of the file.
  if search('^\s*#procedurecall\>','Wc')
    let l:valuePos=matchend(getline('.'),'^\s*#procedurecall\s*')
    let s:procedureCall=strpart(getline('.'),l:valuePos)
    call setline('.','')
  endif

  echo s:procedureCall ? 'Procedure call prefix: '.s:procedureCall : 'No procedure call prefix'

endfunction

function! VimclairRunLabel()

  " Config the auto-run line number.

  " The command '#runLabel' can be used to set the desired label. Only the
  " first occurence of '#runLabel' will be used; it can be anywhere in the
  " source but always at the start of a line (with optional indentation).

  let s:runLabel='' " default value (no auto-run)
  call cursor(1,1) " Go to the top of the file.
  if search('^\s*#runLabel\>','Wc')
    let l:valuePos=matchend(getline('.'),'^\s*#runLabel\s*')
    let s:runLabel=strpart(getline('.'),l:valuePos)
    call setline('.','')
  endif

  echo empty(s:runLabel) ? 'No auto-run' : 'Auto-run label: '.s:runLabel

endfunction

function! VimclairDefine()

  " Search and execute all '#define' directives.

  " There can be any number of '#define' directives, but they must be alone on
  " their own source lines (with optional indentation).

  let s:definedTags=[] " a list for the '#define' tags 

  call cursor(1,1) " Go to the top of the file.
  while search('^\s*#define\>','Wc')
    let l:definition=getline('.')
    let l:tagPos=matchend(l:definition,'^\s*#define\s*')
    let l:tag=strpart(l:definition,l:tagPos)
    if !empty(l:tag)
      call add(s:definedTags,l:tag)
    endif
    call setline('.','')
  endwhile

  let l:tags=len(s:definedTags)
  if l:tags==1
    echo l:tags.' #define directive'
  elseif l:tags>1 
    echo l:tags.' #define directives'
  endif

  call VimclairSaveStep('defined_tags')

endfunction

function! VimclairDefined(needle)

  " Is needle a defined tag?

"  echo "XXX About to search for the <".a:needle."> tag!"
  let l:found=0 " XXX needed, but why? Otherwise, error: undefined variable
  for l:tag in s:definedTags
"      echo 'XXX tag: '.l:tag
      let l:found=(l:tag==a:needle)
      if l:found
          break
      endif
  endfor
  return l:found

endfunction

function! VimclairZXFilename()

  " XXX FIXME check valid filenames

  " Set the filename used inside the TAP file.  Its default value is the
  " name of the Vimclair BASIC source file, but without the filename
  " extension.

  " The command '#filename' can be used to set the desired filename. Only the
  " first occurence of '#filename' will be used; it can be anywhere in the
  " source but always at the start of a line (with optional indentation).

  " Default ZX Spectrum filename: source filename without extension:
  let s:zxFilename=fnamemodify(expand('%'),':t:r')
  
  call cursor(1,1) " Go to the top of the file.
  if search('^\s*#filename\>','Wc')
    let l:valuePos=matchend(getline('.'),'^#filename\s*')
    let s:zxFilename=strpart(getline('.'),l:valuePos)
    call setline('.','')
  endif
    
  let s:zxFilename=strpart(s:zxFilename,0,10) " max length: 10 chars
  echo 'ZX Spectrum filename: '.s:zxFilename

endfunction

function! VimclairTAPMaker()

  " Set the BAS to TAP converter.

  " The '#tapmaker' directive can be used to set the desired
  " filename. Only the first occurence of '#tapmaker' will be
  " used; it can be anywhere in the source but always at the
  " start of a line (with optional indentation).

  " Default 
  let s:tapmaker='bas2tap'
  
  call cursor(1,1) " Go to the top of the file.
  if search('^\s*#bastotap\>','Wc')
    let l:valuePos=matchend(getline('.'),'^#bas2tap\s*')
    let s:tapmaker=strpart(getline('.'),l:valuePos)
    call setline('.','')
  endif
    
  echo 'BAS to TAP converter: '.s:tapmaker

endfunction

" ----------------------------------------------
" Labels and line numbers

function! VimclairLabels()

  " Sintax:

  " Label names must start with '@'. The rest of the name must be letters A-Z,
  " a-z, digits or underscores.

  " A label is defined by putting the label name at the start of a line. If a
  " BASIC command follows it, a colon or a space are required between them.
  
  " The label references are the label names. They will be substituted with
  " the correspondent line number anywhere in the source -- even in text
  " strings!

  let l:ignoreCaseBackup=&ignorecase
  set ignorecase

  " Join every lonely label to its following line
  " (unless its following line has another label definition):
  silent %substitute,^\(@[0-9a-zA-Z_]\+\)\s*:\?\n\([^@]\),\1:\2,ei
  call VimclairSaveStep('labels-joined')

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
    let l:labelValue=line('.')+s:renumLine-1
    " XXX debug check
    "echo '  XXX Clean label: <' . l:label . '> = '.l:labelValue
    " Use the label as the key to its line number:
    let l:lineNumber[l:label]=l:labelValue
    " Go to the next word:
    normal w
  endwhile

  " Remove all label definitions:
  silent! %substitute/^@[0-9a-zA-Z_]\+\s*:\?\s*//ei

  call VimclairSaveStep('label_definitions_removed')

  " Substitute every label reference with its line number:
  for l:label in keys(l:lineNumber)
    "echo 'About to search for label ' l:label
    call cursor(1,1) " Go to the top of the file.
    " Do the subtitution: 
    while search(l:label.'\>','Wc')
      " XXX debug check
"      echo l:label "label reference found"
"      echo 'About to translate it to ' l:lineNumber[l:label]
      "execute 'silent! substitute/\<'.l:label.'\>/'.l:lineNumber[l:label].'/ei'
      execute 'substitute/'.l:label.'\>/'.l:lineNumber[l:label].'/i'
    endwhile
  endfor

  let s:runLine=0 " default: no auto-run
  if !empty(s:runLabel) 
    let s:runLine=l:lineNumber[s:runLabel]
  endif

  let &ignorecase=l:ignoreCaseBackup

  echo 'Labels translated'

  call VimclairSaveStep('labels')
  
endfunction

function! VimclairRenum()

  " Call the nl program (part of the Debian coreutils package):
  execute "silent! %!nl --body-numbering=t --number-format=rn --number-width=5 --number-separator=' ' --starting-line-number=".s:renumLine." --line-increment=1 --body-numbering=a"

  " In older versions of coreutils,
  " -v sets the first line number, and -i sets the line increment.
  " (the long option for -v doesn't work, though the manual mentions it).
  " Modern versions of nl uses the clearer options
  " --first-line and --line-increment, see:
  " http://www.gnu.org/software/coreutils/manual/coreutils.html#nl-invocation

  " Remove spaces before line numbers
  " (nl's --number-width=1 would do this too):
  " XXX TMP commented out for debugging
"  silent! %substitute/^\s*//e
  
  " Remove empty lines
  silent! %substitute/^\s*\d\+\s\+\n//e

  echo 'Line numbers added'

  call VimclairSaveStep('line_numbers')

endfunction

" ----------------------------------------------
" Token conversion

function! VimclairTokens()

  " Modify some tokens to the format required by BAS2TAP.

  if s:tapmaker=='bas2tap'

    silent! %s@\<gosub\>@go sub@ge
    silent! %s@\<goto\>@go to@ge
    silent! %s@\<deffn\>@def fn@ge

  endif

  " Note: The step is saved even if nothing was done,
  " in order to preserve the numbers of the step files
  call VimclairSaveStep('tokens_with_inner_space')

endfunction

" ----------------------------------------------
" Character conversion

function! VimclairChars()

  " Convert special characters

  let l:ignoreCaseBackup=&ignorecase
  set noignorecase

  " Convert BASin format characters

  if s:tapmaker=='zmakebas'

    " Convert characters from BASin format to zmakebas format
    
    " zmakebas already supports the BASin notation for UDG and
    " block graphics characters
    
    " Embedded attributes:
    silent! %s@\\{vn}@\\{20}\\{0}@ge
    silent! %s@\\{vi}@\\{20}\\{1}@ge
    silent! %s@\\{f\([01]\)}@\\{18}\\{\1}@ge
    silent! %s@\\{b\([01]\)}@\\{19}\\{\1}@ge
    silent! %s@\\{p\([0-9]\)}@\\{17}\\{\1}@ge
    silent! %s@\\{i\([0-9]\)}@\\{16}\\{\1}@ge

    " Embedded chars: 
    silent! %s@\\#\(\d\+\)@\\{\1}@g

  elseif s:tapmaker=='bas2tap'

    " Convert characters from BASin format to BAS2TAP format

    " Block graphics (chars 128-143)

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

    " Embedded attributes
    " Note: combined chars (e.g. "\{p7i2b0}") are not converted

    silent! %s@\\{vn}@{INVERSE 0}@ge
    silent! %s@\\{vi}@{INVERSE 1}@ge
    silent! %s@\\{f\([01]\)}@{FLASH \1}@ge
    silent! %s@\\{b\([01]\)}@{BRIGHT \1}@ge
    silent! %s@\\{p\([0-9]\)}@{PAPER \1}@ge
    silent! %s@\\{i\([0-9]\)}@{INK \1}@ge

    " Embedded chars

    silent! %s/\\#\(\d\+\)/\=nr2char(submatch(1))/g

  endif
  
  call VimclairSaveStep('special_chars_converted')

  echo 'Special chars converted'

  let &ignorecase=l:ignoreCaseBackup

endfunction

function! VimclairAddresses2Chars()

  " Convert 16-bit embedded values into two embedded bytes

  " XXX TODO

  while search('\\{[0-9]}','Wc')
  endwhile

endfunction

" ----------------------------------------------
" BAS file

function! VimclairBasfile()

  " Create a copy of the current Vimclair BASIC file
  " with the .bas extension added
  " and open it for editing.

  " Change to the directory of the current file:
  " XXX OLD
"  silent cd %:h

  silent update " Write the current Vimclair BASIC file if needed
  split " Split the window
  let s:basFilename=expand('%:r').'.bas'
  try
  silent execute 'bd '.s:basFilename
  catch /E94:/
  endtry
  silent execute 'write! '.s:basFilename
  silent execute 'edit '.s:basFilename

  echo 'BAS file created'

endfunction

function! VimclairBAS2TAP()

  "   BAS2TAP v2.4 by Martijn van der Heide of ThunderWare Research Center
  "   
  "   Usage: BAS2TAP [-q] [-w] [-e] [-c] [-aX] [-sX] FileIn [FileOut]
  "          -q = quiet: no banner, no progress indication
  "          -w = suppress generation of warnings
  "          -e = write errors to stdout in stead of stderr channel
  "          -c = case independant tokens (be careful here!)
  "          -n = disable syntax checking
  "          -a = set auto-start line in BASIC header
  "          -s = set "filename" in BASIC header

  " XXX TODO check if bas2tap is installed
  " XXX TODO config with more directives
  let s:autorun=s:runLine ? ' -a'.s:runLine : ''
  silent! execute '!bas2tap -c -n'.s:autorun.' -s'.s:zxFilename.' '.s:basFilename.' '.s:tapFilename

endfunction

function! VimclairZmakebas()

  " zmakebas 1.2 (by Russell Marks, 2004-05)
  " 
  " Synopsis:
  "
  " zmakebas [-hlr] [-a startline] [-i incr] [-n speccy_filename]
  "          [-o output_file] [-s line] [input_file]
  "
  " Options:
  "
  " -a : make the generated file auto-start from line startline.
  "      If `-l' was specified, this can be a label, but don't forget
  "      to include the initial `@' to point this out.
  " 
  " -h : give help on command line options.
  " 
  " -i : in labels mode, set line number increment (default 2).
  " 
  " -l : use labels rather than line numbers.
  " 
  " -n : specify filename to use in .TAP file (up to 10 chars),
  "      i.e.  the filename the speccy will see.  Default is a blank
  "      filename (10 spaces).
  " 
  " -o : output to output_file rather than the default `out.tap'.
  "      Use `-' as the filename to output on std out.
  " 
  " -r : write a raw headerless Basic file, rather than the
  "      default .TAP file.
  " 
  " -s : in labels mode, set starting line number (default 10).

  " XXX TODO check if zmakebas is installed

  let s:autorun=s:runLine ? ' -a '.s:runLine : ''
  silent! execute '!zmakebas '.s:autorun.' -n '.s:zxFilename.' -o 's:tapFilename.' '.s:basFilename
  
endfunction

function! VimclairTapFile()
  let s:tapFilename=expand('%:r').'.tap'
  echo "\n"
  if s:tapmaker=='zmakebas'
    call VimclairZmakebas()
  elseif s:tapmaker=='bas2tap'
    call VimclairBAS2TAP()
  else
    echoerr 'Unknown TAP maker: the TAP file was not created'
  endif
endfunction

" ----------------------------------------------
" Generic functions

function! Trim(input_string)
  " Remove trailing spaces from a string.
  " Reference:
  " http://stackoverflow.com/questions/4478891/is-there-a-vimscript-equivalent-for-rubys-strip-strip-leading-and-trailing-s
  return substitute(a:input_string, '^\s*\(.\{-}\)\s*$', '\1', '')
endfunction

" ----------------------------------------------
" Debug

function! XXX(message)
  " XXX TODO
  if true
    echo message
  endif
endfunction

function! VimclairSaveStep(description)

  " Save the current version of the file being converted,
  " into the <./trace/> directory, for debugging purposes.
  " The <./trace/> directory must exist.

  " XXX TODO better, add 0 if s:step<10 
  let l:number='00'.s:step
  let l:number=strpart(l:number,len(l:number)-2)
  " XXX TODO make the trace dir configurable
  silent execute 'write! '.s:stepsDir.s:sourceFilename.'.step_'.l:number.'_'.a:description
  let s:step=s:step+1

endfunction

" ----------------------------------------------
" Main

function! Vbas2tap()

  " Convert the content of the current Vim buffer, a Vimclair
  " BASIC source, to a TAP file with its Sinclair BASIC
  " equivalent.
  "
  " This is the entry function of the converter, the function
  " that has to be called by Vim key mappings (as defined in
  " <~/.vim/ftplugins/vimclairbasic.vim>), manually executed
  " with ':call Vbas2tap()' or called from the provided command
  " line wrapper <vbas2tap.sh>.

  " Save variables that will be changed
  let s:shortmessBackup=&shortmess
  set shortmess=at
  let s:ignoreCaseBackup=&ignorecase
  set ignorecase

  " Counter for the saved step files
  let s:step=0

  " Filename of the source file, without path
  let s:sourceFilename=fnamemodify(expand('%'),':t')

  " Absolute directory of the source file
  let s:sourceFileDir=fnamemodify(expand('%'),':p:h')
  
  " Absolute directory to save the conversion steps into
  let s:stepsDir=s:sourceFileDir.'/vbas2tap_steps/'
  if !isdirectory(s:stepsDir)
    " XXX TODO if exists("*mkdir")
    " XXX TODO catch possible errors
    call mkdir(s:stepsDir,'',0700) 
  endif
  
  echo "\n"
  echo 'vbas2tap (version '.s:version.') by Marcos Cruz (programandala.net)'
  echo "\n"

  " Conversion steps
  call VimclairBasfile()
  call VimclairInclude()
  call VimclairConfig()
  call VimclairConditionalConversion()
  call VimclairClean()
  call VimclairVim()
  call VimclairControlStructures()
  call VimclairLabels()
  call VimclairRenum()
  call VimclairChars()
  call VimclairTokens()

  " Remove the empty lines
  silent! %s/\n$//e

  " Save the final BAS file
  silent w
  silent bw 
  echo 'BAS file saved and closed'

  " Create the TAP file
  call VimclairTapFile()
  
  " Restore the variables that were changed
  let &ignorecase=s:ignoreCaseBackup
  let &shortmess=s:shortmessBackup

endfunction

echo "hola"
echo "hola"
echo "hola"
echo "hola"
echo "hola"
echo "hola"
echo "hola"
echo "hola"
echo "hola"
echo "hola"
echo "hola"

" vim:tw=64:ts=2:sts=2:sw=2:et
