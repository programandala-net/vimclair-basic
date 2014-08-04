" sinbasic2tap.vim

" SinBasic2tap
" Version A-01-201408041604

" Copyright (C) 2014 Marcos Cruz (programandala.net)

" License: XXX TODO

" This file is part of SinBasic:
" http://programandala.net/en.program.sinbasic.html

" ----------------------------------------------
"  Description

" This program, written in Vim Script, is a Sinclair BASIC preprocessor. It
" converts SinBasic source code to an actual Sinclair BASIC program stored in
" a TAP file.
"
" The SinBasic 'language' offers the following advantages over Sinclair BASIC:
"
" - C-style block and line comments.
" - Bash-style line comments.
" - Labels instead of line numbers.
" - Long variable names for strings, arrays and loops (using the #vim
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
"   - IF ... ELSE ... ENDIF
" - Procedures (without parameters):
"   - DEF PROC, END PROC, EXIT PROC, CALL.
" - The CALL command can be changed with
"   '#procedureCall', e.g.:
"     #procedureCall proc
"   Or even make it empty:
"     #procedureCall

" ----------------------------------------------
" To-do list

" 2014-08-01: Improve: The parens used by 'NOT' to enclose the 'WHILE' or
" 'UNTIL' expression may be ommited in certain cases.

" 2014-08-01: 'EXIT DO n', to exit the n-th loop.

" 2014-08-01: Procedures with parameters. Idea:
" Original:
"   myproc 123,"hello"
"   stop
"   defproc myproc arg1,a$
"     print arg1,a$
"   endproc
" Conversion:
"   let arg1=123:let a$="hello":gosub @myproc
"   stop
"   @myproc
"   print arg1,a$
"   return

" 2014-08-04: 'ELSE IF' statement.

" 2014-08-04: 'CASE' structure.

" ----------------------------------------------
" History

" 2014-07-26: Started with the code of BBim2BB
" (http://programandala.net/es.programa.bbim).
" New: 'do...loop', 'do...loop until' and 'do...loop while' implemented.

" 2014-07-27:
" New: First draft of 'do until...loop', 'do while...loop' and nested
" loops.

" 2014-07-31:
" Fix: Some local variables changed to script variables.

" 2014-08-01:
" Fix: All nine combinations of 'do...loop' work fine.
" New: Nested loops finished.
" New: 'exit do' implemented.
" New: 'exit for' implemented.
" New: 'else...endif' implemented.
" New: The TAP file is created (with the bas2tap converter).
" New: Procedures (without parameters): 'defproc', 'endproc', 'exit proc', 'proc'.

" 2014-08-02:
" New: Syntax description of labels. 

" 2014-08-03:
" New: First changes to implement procedure parameters.
" Improvement: The directory of the source file is the working directory.
" This lets '#include' paths relative to it. XXX not tested yet.

" 2014-08-04:
" Improvement: Long 'if...else...endif' structures; the previous
" 'else...endif' method is removed.
" New: '#procedurecall' lets to configure the command used to call the a
" procedure (the default is 'call'), or make it empty.
" Change: Version A-01: first usable version, with loops and long
" conditional structures.

" ----------------------------------------------

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
  call SinBasicExitFor()
  call SinBasicIfEndif()
  call SinBasicProcedures()

endfunction

function! SinBasicDoLoop()

  " Convert all DO...LOOP structures.

  " The SinBasic DO...LOOP structures are copied from Andy Wright's Beta
  " BASIC, SAM BASIC and MasterBASIC: they allow UNTIL and WHILE in any
  " combination (there are nine possible combinations).

  let s:doStatement=''

  echo '--- About to search for a DO!'
  call cursor(1,1)
  while search('^do\(\s\+\(until\|while\)\s\+.\+\)\?$','Wc')
    " first DO found
    echo '--- DO found!'
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

  " Open a DO...LOOP.

  " Syntax:

  " DO and LOOP (with optional UNTIL or WHILE condition) must be the only
  " statements on their lines.

  " How this works:

  " The loop start can be DO, DO UNTIL or DO WHILE.  If it's just DO, a label
  " is enough.  If it's DO UNTIL or DO WHILE, a conditional jump has to be
  " inserted, but the destination line is unknown until the correspondent LOOP
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

  " Close a DO...LOOP.

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

  " Syntax:

  " EXIT DO must be at the end of a line.

  let s:doStatement=''

  echo '--- About to search for an EXIT DO!'
  call cursor(1,1)
  while search('\<exit do$','Wc')
    echo '--- EXIT DO found!'
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

function! SinBasicExitFor()

  " Convert EXIT FOR.

  " Syntax:

  " EXIT FOR and its correspondent NEXT must be at the end of the line.

  let s:doStatement=''

  call cursor(1,1)
  while search('\<exit for$','Wc')
    echo '--- EXIT FOR found at line '.line('.').': '.getline('.')
    let l:exitForLineNumber=line('.')
    if search('\<next [a-z]\>','W')
      echo '--- NEXT found at line '.line('.').': '.getline('.')
      let l:exitLabel='@forExit'.line('.')
      call append('.',l:exitLabel)
      call cursor(l:exitForLineNumber,'^')
      execute 'silent! substitute,\<exit for\>,goto '.l:exitLabel.',ei'
    else
      echo 'Error: EXIT FOR without NEXT at line '.exitForLineNumber
    endif
  endwhile
endfunction

function! SinBasicElse()

  " Convert ELSE...ENDIF.

  " XXX OLD -- Abandoned. This method can not work with nested conditionals.

  " Syntax:

  " ELSE must be at the start of a line. It can be followed by any other
  " command, with or without a separating semicolon.

  " ENDIF must be alone at its own line.
  " It can be written 'END IF' as well.

  call cursor(1,1)
  while search('^else\>','Wc')
    echo '--- ELSE found at line '.line('.').': '.getline('.')
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
      echo '--- IF found at line '.line('.').': '.getline('.')
      let l:exitLabel='@endif'.line('.')
      call setline('.',getline('.').':goto '.l:exitLabel)
      call cursor(l:elseLineNumber,'^')
      if search('^end\s\?if$','W')
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

function! SinBasicIfEndif()

  " Convert all IF...ENDIF structures.

  " XXX TODO finish

  " The SinBasic IF...ENDIF structures are inspired by Andy Wright's Beta
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
  " and the only ELSE of the structure must be on its own line:
  "
  "   IF condition1 THEN
  "     code1
  "   ELSE 
  "     code2
  "   ENDIF

  let s:ifStatement=''

  echo '--- About to search for a long IF!'
  call cursor(1,1)
  while search('^if .\+ then$','Wc')
    " Main long IF found
    echo '--- IF found!'
    let l:ifLineNumber=line('.') " line number of the IF 
    let l:condition=substitute(getline('.'), '^if\s*\(.\{-}\)\s*then$', '\1', '')
    let l:unclosedConditionals=1 " counter
    let l:elseLineNumber=0 " used also as a flag
    while search('^\(if\s\+.\+\s\+then\|else\|end\s*if\)$','W')
      " Nested long IF, ELSE or ENDIF found
      echo '--- IF, ELSE or ENDIF found'
      echo 'line: '.getline('.')
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

  call SinBasicExitDo()

endfunction

function! SinBasicIfCondition(ifLine)
  " Return the condition of a long-IF line
  " (whose format is 'if condition then').
"  let l:condition=strpart(a:ifLine,2)
"  let l:condition=strpart(l:condition,0,len(l:condition)-4)
"  let l:condition=strpart(l:condition,match(l:condition,'\S'))
"  let l:tail=match(l:condition,'then$')
"  let l:condition=strpart(l:condition,0,len(l:condition)-l:tail)
"  return Trim(l:condition)
endfunction

function! SinBasicProcedures()

  " XXX First version, without parameters.

  " Convert DEF PROC, END PROC and EXIT PROC.

  " Syntax:

  " DEF PROC and END PROC must be the only statements of the line.
  " The space is optional: DEFPROC and ENDPROC are valid.
  " EXIT PROC must be at the end of the line.
  " PROC must be used to call a procedure.

  " Description:

  " Procedures are simulated with ordinary routines. No parameters are allowed
  " yet.

  let s:doStatement=''

  call cursor(1,1)
  while search('^def\s\?proc\>','Wc')
    echo '--- DEF PROC found at line '.line('.').': '.getline('.')
    let l:procLineNumber=line('.')
    let l:procLine=getline('.')
    let l:procNamePos=matchend(l:procLine,'^def\s\?proc\s\+')
    let l:procName=strpart(l:procLine,l:procNamePos)
    if len(l:procName)==0
      echo 'Error: DEF PROC bad syntax at line '.l:procLineNumber
    else
      echo 'valid proc name: '.l:procName
      let l:procLabel='@proc'.l:procLineNumber
      echo 'proc label: '.l:procLabel
      call setline(l:procLineNumber,l:procLabel)
      call cursor(l:procLineNumber,'^')
      execute '%substitute,\<'.s:procedureCall.'\s\+'.l:procName.'\>,gosub '.l:procLabel.',gei'
    endif
  endwhile

  silent %substitute,\<exit proc$$,return,ei
  silent %substitute,^end\s\?proc$,return,ei

endfunction

function! XXXSinBasicProcedures()

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
  while search('^def\s\?proc\>','Wc')
    echo '--- DEF PROC found at line '.line('.').': '.getline('.')
    let l:procLineNumber=line('.')
    let l:procLine=getline('.')
    let l:procNamePos=matchend(l:procLine,'^def\s\?proc\s\+')
    let l:procName=strpart(l:procLine,l:procNamePos)
    let l:procEndNamePos=matchend(l:procName,'^\S\+')
    let l:procName=strpart(l:procName,0,l:procEndNamePos)
    echo 'l:procName: '.l:procName
    let l:procParametersPos=matchend(l:procLine,'^def\s\?proc\s\+\S\+\s\+')
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

  silent %substitute,\<exit proc$$,return,ei
  silent %substitute,^end\s\?proc$,return,ei

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
    execute "silent! r ".getcwd().'/'.l:fileName
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
" Config commands

function! SinBasicConfig()

  " Search and parse the config commands.  They can be anywhere in the source
  " but always at the start of a line (with optional indentation).

  " #firstline <line number>
  call SinBasicFirstLine()
  " #procedurecall <command name>
  call SinBasicProcedureCall()

endfunction

function! SinBasicFirstLine()

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

function! SinBasicProcedureCall()

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
  if search('^\s*#procedurecall\s\+[0-9]\+\>','Wc')
    " Store the string into register 'l':
    normal ww"lyw
    " And then into the variable:
    let s:procedureCall=getreg('l',1)
  endif
  echo 'Procedure call: '.s:procedureCall

endfunction

" ----------------------------------------------
" Labels

function! SinBasicLabels()

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

  " Modify some tokens to the format required by bas2tap.

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
  " with the .bas extension added
  " and open it for editing.

  " Change to the directory of the current file:
  silent cd %:h

  silent update " Write the current SinBasic file if needed
  split " Split the window
  let s:basFileName=getreg('%').'.bas'
  silent execute 'write! '.s:basFileName
  silent execute 'edit '.s:basFileName
"  set fileencoding=latin1 " XXX TODO needed?


  echo 'BAS file created.'

endfunction

function! SinBasicTapFile()
  " XXX TODO check if bas2tap is installed
  silent execute '!bas2tap -q -w -c -n '.s:basFileName.' '.s:basFileName.'.tap'
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

" ----------------------------------------------
" Main

function! SinBasic2tap()

  let s:shortmessBackup=&shortmess
  set shortmess=at

  let s:ignoreCaseBackup=&ignorecase
  set ignorecase
  
  call SinBasicConfig()
  call SinBasicBasfile()
  call SinBasicInclude()
  call SinBasicVim()
  call SinBasicClean()
  call SinBasicControlStructures()

" XXX TMP for debugging
  if 1
 
  call SinBasicLabels()
  call SinBasicRenum()
  call SinBasicChars()
  call SinBasicTokens()

  silent w
  silent bw 
  echo 'BAS file saved and closed.'

  call SinBasicTapFile()
  
  endif

  if s:ignoreCaseBackup
    set ignorecase
  else
    set noignorecase
  endif
" XXX TODO
"  set shortmess=l:shortmessBackup

  echo 'Done!'

endfunction

" Shortkey ',sb' in normal mode
" to create a Beta BASIC file:
nmap <silent> ,sb :call SinBasic2tap()<CR>

echo 'SinBasic2tap loaded.'
echo 'Activate it with the key sequence ,sb (comma, S and B), in normal mode, on your SinBasic source.'

" vim:tw=78:ts=2:et:
