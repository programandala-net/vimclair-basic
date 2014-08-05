" vimclair_basic.vim

" Vimclair BASIC
" Version A-03-201408051953

" Copyright (C) 2014 Marcos Cruz (programandala.net)

" License: XXX TODO

" This file is part of Vimclair BASIC
" http://programandala.net/en.program.vimclair_basic.html

" ----------------------------------------------
"  Description

" This program, written in Vim Script, converts a Vimclar BASIC source code to
" an actual Sinclair BASIC program stored in a TAP file.

" Vimclair BASIC offers the following advantages over Sinclair BASIC:
"
" - C-style block and line comments.
" - Bash-style line comments.
" - Labels instead of line numbers.
" - Long variable names for strings, arrays and for loops (using the #vim
"   directive).
" - Control structures:
"   - DO ... LOOP
"   - DO ... LOOP UNTIL
"   - DO ... LOOP WHILE
"   - DO UNTIL... LOOP
"   - DO UNTIL ... LOOP UNTIL
"   - DO UNTIL ... LOOP WHILE
"   - DO WHILE ... LOOP
"   - DO WHILE ... LOOP UNTIL
"   - DO WHILE ... LOOP WHILE
"   - EXIT DO
"   - EXIT FOR
"   - IF ... ELSE IF ... ELSE ... ENDIF
" - Procedures (without parameters):
"   - DEF PROC, END PROC, EXIT PROC, CALL.
"   (Procedures with parameters can simulated ad hoc with the #vim directive.)
" - The CALL command can be changed with '#procedureCall', e.g.:
"     #procedureCall proc
"   Or even make it empty:
"     #procedureCall

" ----------------------------------------------
" To-do and change log

" See the files:
" <vimclair_basic.pending.adoc>
" <vimclair_basic.history.adoc>

" ----------------------------------------------

function! VimclairClean()

  " Clean the source code.

  silent! %s/^\s*#.*$//e " Remove the metacomments
  silent! %s/\s*\/\/.*$//e " Remove the // line comments
  silent! %s,^\s*\/\*\_.\{-}\*\/,,e " Remove the /* */ block comments
  silent! %s/^\s\+//e " Remove indentation
  silent! %s/\s\+$//e " Remove trailing spaces
  silent! %s/^\n//e " Remove the empty lines
  silent! %s/\\\n//e " Join the splitted lines

  echo 'Source code cleaned.'

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

  " Convert all DO...LOOP structures.

  " The Vimclair BASIC DO...LOOP structures are copied from Andy Wright's Beta
  " BASIC, SAM BASIC and MasterBASIC: they allow UNTIL and WHILE in any
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
      echo 'Error: DO without LOOP at line '.s:doLineNumber
    endif
    call cursor(s:doLineNumber,'$')
  endwhile

  call VimclairExitDo()
  
  call VimclairSaveStep('do_loop')

endfunction

function! VimclairDo()

  " Open a DO...LOOP.

  " Syntax:

  " DO and LOOP (with optional UNTIL or WHILE condition) must be the only
  " statements on their lines.

  " How this works:

  " The loop start can be DO, DO UNTIL or DO WHILE.  If it's just DO, a label
  " is enough.  If it's DO UNTIL or DO WHILE, a conditional jump has to be
  " inserted, but the destination line is unknown until the correspondent LOOP
  " is found.  Therefore the code in stored into s:doStatement in order to
  " create it later (VimclairLoop() does it), with the added line number.

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

function! VimclairLoop()

  " Close a DO...LOOP.

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

function! VimclairExitDo()

  " Convert EXIT DO.

  " Syntax:

  " EXIT DO must be at the end of a line.

  let s:doStatement=''

  "echo '  XXX About to search for an EXIT DO!'
  call cursor(1,1)
  while search('\<exit do$','Wc')
    "echo '  XXX EXIT DO found!'
    let l:exitDoLineNumber=line('.')
    if search('^@loopExit\d\+$','W')
      let l:exitLabel=getline('.')
      call cursor(l:exitDoLineNumber,'^')
      execute 'silent! substitute,\<exit do\>,goto '.l:exitLabel.',ei'
    else
      echo 'Error: EXIT DO without LOOP at line '.exitDoLineNumber
    endif
  endwhile
endfunction

function! VimclairExitFor()

  " Convert EXIT FOR.

  " Syntax:

  " EXIT FOR and its correspondent NEXT must be at the end of the line.

  let s:doStatement=''

  call cursor(1,1)
  while search('\<exit for$','Wc')
    "echo '  XXX EXIT FOR found at line '.line('.').': '.getline('.')
    let l:exitForLineNumber=line('.')
    if search('\<next [a-z]\>','W')
      "echo '  XXX NEXT found at line '.line('.').': '.getline('.')
      let l:exitLabel='@forExit'.line('.')
      call append('.',l:exitLabel)
      call cursor(l:exitForLineNumber,'^')
      execute 'silent! substitute,\<exit for\>,goto '.l:exitLabel.',ei'
    else
      echo 'Error: EXIT FOR without NEXT at line '.exitForLineNumber
    endif
  endwhile
  
  call VimclairSaveStep('exit_for')

endfunction

function! VimclairElse()

  " Convert ELSE...ENDIF.

  " XXX OLD -- Abandoned. This method can not work with nested conditionals.

  " Syntax:

  " ELSE must be at the start of a line. It can be followed by any other
  " command, with or without a separating semicolon.

  " ENDIF must be alone at its own line.
  " It can be written 'END IF' as well.

  call cursor(1,1)
  while search('^else\>','Wc')
    "echo '  XXX ELSE found at line '.line('.').': '.getline('.')
    let l:elseLineNumber=line('.')
    let l:elseLine=getline('.')
    let l:elsePartPos=matchend(l:elseLine,'else\(\(\s\|:\)\+\)\?')
    let l:elsePart=strpart(l:elseLine,l:elsePartPos)
    echo "l:elsePart: ".l:elsePart
    call setline('.',l:elsePart) " remove the ELSE
    if len(l:elsePart)==0
      silent substitute,\n,,
    endif
    if search('\<if .\+ then\>','Wb')
      "echo '  XXX IF found at line '.line('.').': '.getline('.')
      let l:exitLabel='@endif'.line('.')
      call setline('.',getline('.').':goto '.l:exitLabel)
      call cursor(l:elseLineNumber,'^')
      if search('^end\s*if$','W')
        call setline('.',l:exitLabel)
      else
        echo 'Error: ELSE without ENDIF at line '.elseLineNumber
      endif
    else
      echo 'Error: ELSE without IF at line '.elseLineNumber
    endif
    call cursor(l:elseLineNumber,'^')

  endwhile
endfunction

function! EXVimclairIfEndif()

  " Convert all IF...ENDIF structures.

  " XXX OLD Second version, without ELSE IF or nesting.

  " The Vimclair BASIC IF...ENDIF structures are inspired by Andy Wright's Beta
  " BASIC, SAM BASIC and MasterBASIC, but they are not identical.

  " Syntax:
  "
  " Short IF structures are the same than Sinclair BASIC's.
  "
  "   IF condition THEN action
  "
  " Of course they can be splitted into any number of text lines:
  " 
  "   IF condition THEN \
  "     action
  "
  " As usual, the splitting format does not affect the parsing,
  " as long as the required spaces are preserved at the splitting points:
  "
  "   IF \
  "     condition \
  "   THEN \
  "     action
  "
  " Long IF structures must have 'THEN' at the end of the actual source line,
  " and the ELSE must be on its own line:
  "
  "   IF condition1 THEN
  "     code1
  "   ELSE 
  "     code2
  "   ENDIF

  let s:ifStatement=''

  "echo '  XXX About to search for a long IF!'
  call cursor(1,1)
  while search('^if .\+ then$','Wc')
    " Main long IF found
    "echo '  XXX IF found!'
    let l:ifLineNumber=line('.') " line number of the IF 
    let l:condition=substitute(getline('.'), '^if\s*\(.\{-}\)\s*then$', '\1', '')
    let l:unclosedConditionals=1 " counter
    let l:elseLineNumber=0 " used also as a flag
    while search('^\(if\s\+.\+\s\+then\|else\|end\s*if\)$','W')
      " Nested long IF, ELSE or ENDIF found
      "echo '  XXX IF, ELSE or ENDIF found'
      "echo '  XXX line: '.getline('.')
      if strpart(getline('.'),0,2)=='if'
        " Nested long IF
        let l:unclosedConditionals=l:unclosedConditionals+1
      elseif strpart(getline('.'),0,4)=='else'
        " ELSE
        if l:unclosedConditionals==1 " current?
          let l:elseLineNumber=line('.')
          let l:elseLabel='@else'.l:ifLineNumber
          call setline('.',l:elseLabel)
        endif
      else
        " ENDIF
        let l:unclosedConditionals=l:unclosedConditionals-1
        if l:unclosedConditionals==0 " current?
          let l:endifLabel='@endif'.l:ifLineNumber
          call setline('.',l:endifLabel)
          if l:elseLineNumber " there was an ELSE?
            call append(l:elseLineNumber-1,'goto '.l:endifLabel)
            " The IF must jump to ELSE
            let l:newIf='if not('.l:condition.') then goto '.l:elseLabel
          else
            " The IF must jump to ENDIF
            let l:newIf='if not('.l:condition.') then goto '.l:endifLabel
          endif
          call setline(l:ifLineNumber,l:newIf)
          break
        endif
      endif
    endwhile
    if l:unclosedConditionals
      echo 'Error: IF without ENDIF at line '.ifLineNumber
    endif
    call cursor(l:ifLineNumber,'$')
  endwhile

  call VimclairExitDo()

endfunction

function! VimclairIfEndif()

  " Convert all IF...ENDIF structures.

  " XXX TODO finish

  " The Vimclair BASIC IF...ENDIF structures are inspired by Andy Wright's Beta
  " BASIC, SAM BASIC and MasterBASIC, but they are not identical.

  " Syntax:
  "
  " Short IF structures are the same than Sinclair BASIC's.
  "
  "   IF condition THEN action
  "
  " Of course they can be splitted into any number of text lines:
  " 
  "   IF condition THEN \
  "     action
  "
  " As usual, the splitting format does not affect the parsing,
  " as long as the required spaces are preserved at the splitting points:
  "
  "   IF \
  "     condition \
  "   THEN \
  "     action
  "
  " Long IF structures must have 'THEN' at the end of the actual source line;
  " ELSE IF must be at the start of the line; ELSE must be on its own line:
  "
  "   IF condition1 THEN
  "     code1
  "   ELSE IF condition2
  "     code2
  "   ELSE 
  "     code3
  "   ENDIF

  let s:ifStatement=''

  "echo '  XXX About to search for a long IF!'
  call cursor(1,1)
  while search('^if .\+ then$','Wc')

    " Main long IF found
    "echo '  XXX IF found!'
    let l:ifLineNumber=line('.')
    let l:endifLabel='@endif'.l:ifLineNumber
    let l:conditionLineNumber=line('.')
    let l:condition=substitute(getline('.'), '^if\s*\(.\{-}\)\s*then$', '\1', '')
    let l:unclosedConditionals=1 " counter
    let l:elseLineNumber=0 " used also as a flag

    while search('^\(\(else\s\+\)\?if\s\+.\+\s\+then\|else\|end\s*if\)$','W')
      " Nested long IF, ELSE IF, ELSE or ENDIF found
      "echo '  XXX IF, ELSE or ENDIF found'
      "echo '  XXX line: '.getline('.')
      if strpart(getline('.'),0,2)=='if'
        " Nested long IF
        let l:unclosedConditionals=l:unclosedConditionals+1
      elseif strpart(getline('.'),0,7)=='else if'
        " ELSE IF
        if l:unclosedConditionals==1 " current IF structure?
          if l:elseLineNumber " there was a previous ELSE?
            echo 'Error: ELSE IF after ELSE at line '.line('.')
            break
          else
            call append(line('.')-1,'goto '.l:endifLabel)
            " Make the previous condition jump here when false:
            let l:elseIfLabel='@elseIf'.l:ifLineNumber.'_'.line('.')
            let l:newIf='if not('.l:condition.') then goto '.l:elseIfLabel
            call setline(l:conditionLineNumber,l:newIf)
            call append(line('.')-1,l:elseIfLabel)
            " Keep the current condition:
            let l:conditionLineNumber=line('.')
            let l:condition=substitute(getline('.'), '^else\s\+if\s*\(.\{-}\)\s*then$', '\1', '')
            " 
          endif
        endif
      elseif strpart(getline('.'),0,4)=='else'
        " ELSE
        if l:unclosedConditionals==1 " current IF structure?
          if l:elseLineNumber " there was a previous ELSE?
            echo 'Error: Second ELSE at line '.line('.')
            break
          else
            call append('.'-1,'goto '.l:endifLabel)
            let l:elseLineNumber=line('.')
            " Make the previous condition jump here when false:
            let l:elseLabel='@else'.l:ifLineNumber
            let l:newIf='if not('.l:condition.') then goto '.l:elseLabel
            call setline(l:conditionLineNumber,l:newIf)
            call setline('.',l:elseLabel)
            " Keep the current condition:
            let l:conditionLineNumber=line('.')
            let l:condition=''
          endif
        endif
      else
        " ENDIF
        let l:unclosedConditionals=l:unclosedConditionals-1
        if l:unclosedConditionals==0 " current IF structure?
          call setline('.',l:endifLabel)
          if len(l:condition) " is there an unresolved condition?
            let l:newIf='if not('.l:condition.') then goto '.l:endifLabel
            call setline(l:conditionLineNumber,l:newIf)
          endif
          break
        endif
      endif
    endwhile

    if l:unclosedConditionals
      echo 'Error: IF without ENDIF at line '.ifLineNumber
    endif

    call cursor(l:ifLineNumber,'$')
  endwhile

  call VimclairSaveStep('if_endif')

endfunction

function! VimclairIfCondition(ifLine)
  " Return the condition of a long-IF line
  " (whose format is 'if condition then').
"  let l:condition=strpart(a:ifLine,2)
"  let l:condition=strpart(l:condition,0,len(l:condition)-4)
"  let l:condition=strpart(l:condition,match(l:condition,'\S'))
"  let l:tail=match(l:condition,'then$')
"  let l:condition=strpart(l:condition,0,len(l:condition)-l:tail)
"  return Trim(l:condition)
endfunction

function! VimclairProcedures()

  " XXX First version, without parameters.

  " Convert DEF PROC, END PROC and EXIT PROC.

  " Syntax:

  " DEF PROC and END PROC must be the only statements of the line.
  " The space is optional: DEFPROC and ENDPROC are valid.
  " EXIT PROC must be at the end of the line.
  " CALL must be used to call a procedure, but it can be changed
  " to anything (even to an empty string) with '#procedureCall'.

  " Description:

  " Procedures are simulated with ordinary routines. No parameters are allowed
  " yet.

  let s:doStatement=''

  call cursor(1,1)
  while search('^def\s*proc\>','Wc')
    "echo '  XXX DEF PROC found at line '.line('.').': '.getline('.')
    let l:procLineNumber=line('.')
    let l:procLine=getline('.')
    let l:procNamePos=matchend(l:procLine,'^def\s*proc\s\+')
    let l:procName=strpart(l:procLine,l:procNamePos)
    if len(l:procName)==0
      echo 'Error: DEF PROC without name at line '.l:procLineNumber
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

  silent! %substitute,\<exit\s*proc$,return,ei
  silent! %substitute,^end\s*proc$,return,ei

  call VimclairSaveStep('procedures')
  
endfunction

function! XXXVimclairProcedures()

  " XXX second version, with parameters, unfinished

  " Convert DEF PROC, END PROC and EXIT PROC.

  " Syntax:

  " DEF PROC and END PROC must be the only statements of the line.
  " The space is optional: DEFPROC and ENDPROC are valid.
  " EXIT PROC must be at the end of the line.
  " PROC must be used to call a procedure.

  " Description:

  " Procedures are simulated with routines.
  " Parameters are simulated with LET before calling the subroutines.

  let s:doStatement=''
  let l:procParametersList=[]
  let s:maxProcParameters=2
  "let l:procParametersPattern='\(\a[a-zA-Z0-9_]*\$\?,\?\)\{-'+s:maxProcParameters+'}'
  let l:procParametersPattern='\(\a[a-zA-Z0-9_]*\$\?\)'
  let l:i=1
  while l:i<s:maxProcParameters
    let l:procParametersPattern=l:procParametersPattern.',\(\a[a-zA-Z0-9_]*\$\?\)'
    let l:i=l:i+1
  endwhile
  echo 'l:procParametersPattern: '.l:procParametersPattern

  call cursor(1,1)
  while search('^def\s*proc\>','Wc')
    "echo '  XXX DEF PROC found at line '.line('.').': '.getline('.')
    let l:procLineNumber=line('.')
    let l:procLine=getline('.')
    let l:procNamePos=matchend(l:procLine,'^def\s*proc\s\+')
    let l:procName=strpart(l:procLine,l:procNamePos)
    let l:procEndNamePos=matchend(l:procName,'^\S\+')
    let l:procName=strpart(l:procName,0,l:procEndNamePos)
    echo 'l:procName: '.l:procName
    let l:procParametersPos=matchend(l:procLine,'^def\s*proc\s\+\S\+\s\+')
    echo 'l:procParametersPos: '.l:procParametersPos
    if l:procParametersPos>-1
      let l:procParameters=strpart(l:procLine,l:procParametersPos)
      echo 'l:procParameters: '.l:procParameters
    endif
    if len(l:procName)==0
      echo 'Error: DEF PROC bad syntax at line '.l:procLineNumber
    else
      echo 'valid proc name: '.l:procName
      let l:procLabel='@proc'.l:procLineNumber
      echo 'proc label: '.l:procLabel
      call setline(l:procLineNumber,l:procLabel)
      if l:procParametersPos>-1
        let l:procParametersList=matchlist(l:procParameters,l:procParametersPattern)
        echo 'l:procParametersList:'
        echo l:procParametersList
      endif
      call cursor(l:procLineNumber,'^')
      " XXX TODO make PROC optional in the calls:
      execute '%substitute,\<proc '.l:procName.'\>,gosub '.l:procLabel.',gei'
    endif
  endwhile

  silent %substitute,\<exit\s*proc$$,return,ei
  silent %substitute,^end\s*proc$,return,ei

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

  call VimclairSaveStep('vim_commands')
  
endfunction

function! VimclairInclude()

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
    execute "silent! r ".getcwd().'/'.l:fileName
  endwhile

  if l:includedFiles==0
    echo 'No file included.'
  elseif l:includedFiles==1
    echo 'One file included.'
  else
    echo l:includedFiles 'files included.'
  endif

  call VimclairSaveStep('included_files')
  
endfunction

" ----------------------------------------------
" Config commands

function! VimclairConfig()

  " Search and parse the config commands.  They can be anywhere in the source
  " but always at the start of a line (with optional indentation).

  " #firstline <line number>
  call VimclairFirstLine()
  " #procedurecall <command name>
  call VimclairProcedureCall()
  
  call VimclairSaveStep('config_values')

endfunction

function! VimclairFirstLine()

  " Store into s:firstLine the first line number
  " to be used by the final Sinclair BASIC program.
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

function! VimclairProcedureCall()

  " Store into s:procedureCall the command
  " used in the source
  " by the Sinclair BASIC program.
  " The command #procedureCall can be used to set
  " the desired line number. Only the first occurence
  " of #procedureCall will be used; it can be anywhere
  " in the source but always at the start of a line
  " (with optional indentation).

  let s:procedureCall='call' " default value
  
  call cursor(1,1) " Go to the top of the file.
  if search('^\s*#procedurecall\>','Wc')
    let l:valuePos=matchend(getline('.'),'^\s*#procedurecall\>')
    let s:procedureCall=strpart(getline('.'),l:valuePos)
  endif
  echo 'Procedure call: '.s:procedureCall

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
" XXX TMP commented out for debugging
"  silent %substitute,^\(@[0-9a-zA-Z_]\+\)\s*:\?\n\([^@]\),\1:\2,ei
"  call VimclairSaveStep('labels-joined')

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
    let l:labelValue=line('.')+s:firstLine-1
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

  call VimclairSaveStep('labels_substituted')

  let &ignorecase=l:ignoreCaseBackup

  echo 'Labels translated.'

  call VimclairSaveStep('labels')
  
endfunction

function! VimclairRenum()

  " Call the nl program (part of the Debian coreutils package):
  execute "silent! %!nl --body-numbering=t --number-format=rn --number-width=5 --number-separator=' ' --starting-line-number=".s:firstLine." --line-increment=1 --body-numbering=a"

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

  echo 'Line numbers added.'

  call VimclairSaveStep('line_numbers')

endfunction

" ----------------------------------------------
" Token conversion

function! VimclairTokens()

  " Modify some tokens to the format required by BAS2TAP.

  silent! %s@\<gosub\>@go sub@ge
  silent! %s@\<goto\>@go to@ge
  silent! %s@\<deffn\>@def fn@ge

  call VimclairSaveStep('tokens_with_inner_space')

endfunction

" ----------------------------------------------
" Character translation

function! VimclairGraphs()

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

  call VimclairSaveStep('chars_from_basin_to_bas2tap')

endfunction

function! VimclairChars()

  let l:ignoreCaseBackup=&ignorecase
  set noignorecase

  call VimclairGraphs()

  " Embedded ASCII codes (BASin format):
  silent! %s/\\#\(\d\+\)/\=nr2char(submatch(1))/g

  echo 'Special chars translated.'

  call VimclairSaveStep('embedded_chars_in_basin_format')
  
  let &ignorecase=l:ignoreCaseBackup

endfunction

" ----------------------------------------------
" BAS file

function! VimclairBasfile()

  " Create a copy of the current Vimclair BASIC file
  " with the .bas extension added
  " and open it for editing.

  " Change to the directory of the current file:
  silent cd %:h

  silent update " Write the current Vimclair BASIC file if needed
  split " Split the window
  let s:basFileName=getreg('%').'.bas'
  silent execute 'write! '.s:basFileName
  silent execute 'edit '.s:basFileName
"  set fileencoding=latin1 " XXX TODO needed?

  echo 'BAS file created.'

  call VimclairSaveStep('bas_file')

endfunction

function! VimclairTapFile()

  " XXX TODO check if bas2tap is installed

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

  " XXX TODO config with directives
  " XXX TODO show possible errors
  execute '!bas2tap -q -c -n '.s:basFileName.' '.s:basFileName.'.tap'
  echo 'TAP file created.'

endfunction

" ----------------------------------------------
" Generic functions

" XXX TMP
function! Trim(input_string)
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
  " for debugging purposes.
 
  let l:number='00'.s:step
  let l:number=strpart(l:number,len(l:number)-2)
  silent execute 'write! '.getreg('%').'.step_'.l:number.'_'.a:description
  let s:step=s:step+1

endfunction

" ----------------------------------------------
" Main

function! VimclairBASIC()

  let s:shortmessBackup=&shortmess
  set shortmess=at
  let s:ignoreCaseBackup=&ignorecase
  set ignorecase

  let s:step=0 " counter for the saved step files

  echo "Converting Vimclair BASIC to Sinclair BASIC..." 
  call VimclairBasfile()
  call VimclairConfig()
  call VimclairInclude()
  call VimclairVim()
  call VimclairClean()
  call VimclairControlStructures()

" XXX TMP for debugging
  if 1
 
  call VimclairLabels()
  call VimclairRenum()
  call VimclairChars()
  call VimclairTokens()

  silent w
  silent bw 
  echo 'BAS file saved and closed.'

  call VimclairTapFile()
  
  endif

  let &ignorecase=s:ignoreCaseBackup
  let &shortmess=s:shortmessBackup

  echo 'Done!'

endfunction

" Shortkey ',sb' in normal mode
" to create a Beta BASIC file:
nmap <silent> ,vb :call VimclairBASIC()<CR>

echo 'Vimclair BASIC converter'
echo '========================'
echo 'While you are editing your Vimclair BASIC source,'
echo 'you can run the converter just typing the following 3 keys'
echo '(in Vim normal mode): ,vb'

" vim:tw=78:ts=2:sts=2:et:
