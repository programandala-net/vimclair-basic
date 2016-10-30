" vimclairbasic.vim
" Vim syntax file
" Language:    Vimclair BASIC (for the ZX Spectrum computer)
" Author:      Marcos Cruz (programandala.net)
" License:     Vim license (GPL compatible)
" URL:         http://programandala.net/en.program.vimclair_basic.html
" Updated:     2016-10-30

" -------------------------------------------------------------
" Change history: see at the end of the file

" -------------------------------------------------------------
" To-do

" Finish the colors

" Test everything.

" Fix: the code commented out is highlighted as metacomments when tabs are
" used for indentation.

" 2014-08-08: Fix: label definitions group.

" 2014-12-13: Why PreProc keywords are highighted only at the start of the
" line?

" -------------------------------------------------------------

" For Vim version 5.x: Clear all syntax items
" For Vim version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if version > 600
  setlocal iskeyword=48-57,65-90,97-122,_,$,#
else
  set iskeyword=48-57,65-90,97-122,_,$,#
endif

syn case ignore

" -------------------------------------------------------------

" Directives

syn keyword vimclairbasicPreProc #define
syn keyword vimclairbasicPreProc #endif
syn keyword vimclairbasicPreProc #else
syn keyword vimclairbasicPreProc #filename
syn keyword vimclairbasicPreProc #ifdef
syn keyword vimclairbasicPreProc #ifndef
syn keyword vimclairbasicPreProc #include
syn keyword vimclairbasicPreProc #previm
syn keyword vimclairbasicPreProc #procedurecall
syn keyword vimclairbasicPreProc #renumline
syn keyword vimclairbasicPreProc #run
syn keyword vimclairbasicPreProc #tapmaker
syn keyword vimclairbasicPreProc #vim

" Functions

syn keyword vimclairbasicFunction ABS
syn keyword vimclairbasicFunction ACS
syn keyword vimclairbasicFunction ASN
syn keyword vimclairbasicFunction ATN
syn keyword vimclairbasicFunction ATTR
syn keyword vimclairbasicFunction CHR$
syn keyword vimclairbasicFunction CODE
syn keyword vimclairbasicFunction COS
syn keyword vimclairbasicFunction EXP
syn keyword vimclairbasicFunction FN
syn keyword vimclairbasicFunction IN
syn keyword vimclairbasicFunction INKEY$
syn keyword vimclairbasicFunction INT
syn keyword vimclairbasicFunction LEN
syn keyword vimclairbasicFunction LN
syn keyword vimclairbasicFunction NOT
syn keyword vimclairbasicFunction PEEK
syn keyword vimclairbasicFunction PI
syn keyword vimclairbasicFunction POINT
syn keyword vimclairbasicFunction RND
syn keyword vimclairbasicFunction SCREEN$
syn keyword vimclairbasicFunction SGN
syn keyword vimclairbasicFunction SIN
syn keyword vimclairbasicFunction SQR
syn keyword vimclairbasicFunction STR$
syn keyword vimclairbasicFunction TAN
syn keyword vimclairbasicFunction USR
syn keyword vimclairbasicFunction VAL
syn keyword vimclairbasicFunction VAL$

" Qualifiers

syn keyword vimclairbasicQualifier AT
syn keyword vimclairbasicQualifier LINE
syn keyword vimclairbasicQualifier STEP
syn keyword vimclairbasicQualifier TAB
syn keyword vimclairbasicQualifier TO

" Structure

syn match vimclairbasicStructure "\<DEF \?FN\>"
syn match vimclairbasicStructure "\<DEF \?PROC\>"
syn keyword vimclairbasicStructure CALL
syn keyword vimclairbasicStructure PROC
syn keyword vimclairbasicStructure DO
syn keyword vimclairbasicStructure ELSE
syn keyword vimclairbasicStructure ELSEIF
syn match vimclairbasicStructure "\<END \?IF\>"
syn match vimclairbasicStructure "\<END \?PROC\>"
syn match vimclairbasicStructure "\<EXIT \?DO\>"
syn match vimclairbasicStructure "\<EXIT \?FOR\>"
syn match vimclairbasicStructure "\<EXIT \?PROC\>"
syn keyword vimclairbasicStructure FOR
syn match vimclairbasicStructure "\<GO \?SUB\>"
syn match vimclairbasicStructure "\<GO \?TO\>"
syn keyword vimclairbasicStructure IF
syn keyword vimclairbasicStructure LOOP
syn keyword vimclairbasicStructure NEXT
syn keyword vimclairbasicStructure STOP
syn keyword vimclairbasicStructure THEN
syn keyword vimclairbasicStructure UNTIL
syn keyword vimclairbasicStructure WHILE

" Commands

syn keyword vimclairbasicCommand BEEP
syn keyword vimclairbasicCommand BORDER
syn keyword vimclairbasicCommand BRIGHT
syn keyword vimclairbasicCommand CIRCLE
syn keyword vimclairbasicCommand CLEAR
syn keyword vimclairbasicCommand CLOSE
syn keyword vimclairbasicCommand CLS
syn keyword vimclairbasicCommand CONTINUE
syn keyword vimclairbasicCommand COPY
syn keyword vimclairbasicCommand DATA
syn keyword vimclairbasicCommand DIM
syn keyword vimclairbasicCommand DRAW
syn keyword vimclairbasicCommand ERASE
syn keyword vimclairbasicCommand FLASH
syn keyword vimclairbasicCommand FORMAT
syn keyword vimclairbasicCommand INK
syn keyword vimclairbasicCommand INPUT
syn keyword vimclairbasicCommand INVERSE
syn keyword vimclairbasicCommand LABEL
syn keyword vimclairbasicCommand LET
syn keyword vimclairbasicCommand LIST
syn keyword vimclairbasicCommand LLIST
syn keyword vimclairbasicCommand LOAD
syn keyword vimclairbasicCommand LPRINT
syn keyword vimclairbasicCommand MERGE
syn keyword vimclairbasicCommand MOVE
syn keyword vimclairbasicCommand NEW
syn keyword vimclairbasicCommand OPEN
syn keyword vimclairbasicCommand OUT
syn keyword vimclairbasicCommand OVER
syn keyword vimclairbasicCommand PAPER
syn keyword vimclairbasicCommand PAUSE
syn keyword vimclairbasicCommand PLOT
syn keyword vimclairbasicCommand POKE
syn keyword vimclairbasicCommand PRINT
syn keyword vimclairbasicCommand RANDOMIZE
syn keyword vimclairbasicCommand READ
syn keyword vimclairbasicCommand RESTORE
syn keyword vimclairbasicCommand RETURN
syn keyword vimclairbasicCommand RUN
syn keyword vimclairbasicCommand SAVE
syn keyword vimclairbasicCommand VERIFY
syn keyword vimclairbasicMathsOperator AND
syn keyword vimclairbasicMathsOperator BIN
syn keyword vimclairbasicMathsOperator OR

" ZX Spectrum 128K commands

syn keyword vimclairbasicCommand PLAY
syn keyword vimclairbasicCommand SPECTRUM

" -------------------------------------------------------------

"integer number, or floating point number without a dot.
syn match  vimclairbasicNumber "\<\d\+\>"
"floating point number, with dot
syn match  vimclairbasicNumber "\<\d\+\.\d*\>"
"floating point number, starting with a dot
syn match  vimclairbasicNumber "\.\d\+\>"
"hex number XXX TODO -- and also 0x notation
"syn match  vimclairbasicNumber "&[0-9a-f]\+\>"

" String and Character constants
syn match vimclairbasicSpecial contained "\\\d\d\d\|\\."
"syn region vimclairbasicString start='"' skip='""' end='"' contains=vimclairbasicSpecial
syn region vimclairbasicString start='"' skip='""' end='"'

"syn match   vimclairbasicTypeSpecifier "[a-zA-Z0-9][\$]"ms=s+1
syn match   vimclairbasicMathsOperator "[<>+\*^/=-]\|>=\|=<\|<>]"

" Comments

syn keyword vimclairbasicTodo contained todo fixme xxx
syn region vimclairbasicLineStartComment start="^\s*//\s" end="$" contains=vimclairbasicTodo
syn region  vimclairbasicComment start="\(^\|:\)\s*REM\>" end="$" contains=vimclairbasicTodo
"syn region vimclairbasicMetaComment start="^\s*#[# ]*" end="$" contains=vimclairbasicTodo
syn region vimclairbasicMetaComment start="^\s*#\s" end="$" contains=vimclairbasicTodo
syn match vimclairbasicLineEmptyComment "^\s*//\s*$"
syn region vimclairbasicLineEndComment start="\s\+//\s" end="$" contains=vimclairbasicTodo
syn region vimclairbasicBlockComment start="^\s*/\*" end="\*/" contains=vimclairbasicTodo 

syn match vimclairbasicLabel "^\s*@[a-zA-Z0-9_]\+\>"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_basic_syntax_inits")
  if version < 508
    let did_basic_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink vimclairbasicNumber Number
  HiLink vimclairbasicCommand Statement
  HiLink vimclairbasicQualifier Statement
  HiLink vimclairbasicStructure Structure
  HiLink vimclairbasicString String
  HiLink vimclairbasicComment Comment
  HiLink vimclairbasicBlockComment Comment
  HiLink vimclairbasicLineStartComment Comment
  HiLink vimclairbasicMetaComment Comment
  HiLink vimclairbasicLineEndComment Comment
  HiLink vimclairbasicLineEmptyComment Comment
  HiLink vimclairbasicSpecial Special
  HiLink vimclairbasicTodo Todo
  HiLink vimclairbasicFunction Function
"  HiLink vimclairbasicTypeSpecifier Type
"  hi vimclairbasicMathsOperator term=bold cterm=bold gui=bold
  HiLink vimclairbasicMathsOperator Operator
  HiLink vimclairbasicPreProc PreProc
  HiLink vimclairbasicLabel Label

  delcommand HiLink
endif

let b:current_syntax = "vimclairbasic"

" -------------------------------------------------------------
" Change history

" 2014-07-26: First version
" (based on masterbasic.vim and mbim.vim, by the same author).

" 2014-08-01: Added DO LOOP, ENDIF, DEF PROC, END PROC, PROC, EXIT... after
" the improvements in the language.

" 2014-08-02: Fix: now 'rem' is highlighted only at the start of the line or
" after a colon.

" 2014-08-02: New: '...LineEmptyComment'.

" 2014-08-05: Renamed. All label names are updated.

" 2014-08-08: Fix: the line comment '//' was not highlighted at the start of
" the line, and the block comment was not highlighted at all; also the preproc
" directives were not highlighted. The reason was the
" vimclairbasicTypeSpecifier and vimclairbasicMathsOperator were defined below
" the comments. They are moved above comments. Anyway,
" vimclairbasicTypeSpecifier is commented out.

" 2014-08-08: New: label definitions.

" 2014-08-09: Directives are updated after the changes in the language.

" 2014-10-20: New: '#define'.

" 2014-10-22: Change: '#runline' to '#runlabel', after the changes in the
" language.
"
" 2014-12-13: New: '#ifdef', '#ifndef', '#else', '#endif'.

" 2014-12-13: Change: The metacomments are updated after the changes in the
" language: now they must have at least one following space. This makes some
" things easier and prevents possible name clashes with future directives.

" 2015-02-09: Change: All unused keywords are removed, but noted in the to-do
" list of the project.

" 2015-02-26: Vim license.
"
" 2015-02-28: New: '#tapmaker'.
"
" 2015-03-01: Change: '#runlabel' to '#run', after the changes
" in the language.
"
" 2015-03-01: Fix: 'MOVE', 'PLAY' and 'SPECTRUM' were missing.
"
" 2015-03-07: Fix: now 'REM' is highligted also when nothing follows it.
"
" 2015-03-19: New: '#previm'.
"
" 2016-10-30: Remove unused highlighting.
