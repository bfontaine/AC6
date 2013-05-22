" Vim syntax file
" Language:     Clap
" Filenames:    *.clap
" Maintainers:  Baptiste Fontaine <b@ptistefontaine.fr>
" URL:          -
" Last Change:  2013 May 15 - Creation
"
" Some parts of this file come from Vim's official OCaml syntax file

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax") && b:current_syntax == "clap"
  finish
endif

" Clap is case sensitive.
syn case match

syn match    clapCommentErr "\*)"
syn match    clapThenErr    "\<then\>"

" Comments
syn region   clapComment start="(\*" end="\*)" contains=clapComment,clapTodo
syn region   clapLineComment start="\*\*" end="$" oneline contains=clapTodo
syn keyword  clapTodo contained TODO FIXME XXX NOTE

" Non-standard
syn match    clapComment "\%^#!.*"

" If
" syn region clapNone matchgroup=clapKeyword start="\<if\>" matchgroup=clapKeyword end="\<then\>" contains=ALLBUT,clapThenErr

" types
syn cluster clapType contains=clapIdentifierLC

" constructors
syn cluster clapConstructor contains=clapIdentifierHC

syn cluster clapKeywords contains=clapAt,clapKeyword

syn keyword clapAt      at
syn keyword clapKeyword and case def
syn keyword clapKeyword do else end fun
syn keyword clapKeyword if in is not or
syn keyword clapKeyword rec then type val
syn keyword clapKeyword where with

syn match clapColon ":"

syn keyword clapBoolean True False

syn match    clapIdentifierLC "[a-z]\w*"
syn match    clapIdentifierHC "[A-Z]\w*"

syn match    clapCharacter    "'[\x20-\x26\x28-\x5B\x5D-\x7E]'"
syn match    clapCharacter    "'\\[\\ntbr']'"
syn match    clapCharacter    "'\\0x\x\x'"
syn match    clapCharacter    "'\\\([01]\d\d\|2[0-4]\d|25[0-5]\)'"

syn match    clapCharErr      "'[\x01-\x19\x27\x5C\x5D]'"
syn match    clapCharErr      "'\\[^\'ntbr]'"
syn match    clapCharErr      "'\\0x\x\x\x\+'"
syn match    clapCharErr      "'\\\([13-9]\d\d\|25[6-9]\|2[0-46-9]\d\|\d\d\d\d\+\)'"

syn region   clapString       start=+"+ skip=+\\\\\|\\"+ end=+"+


syn match    clapRefAssign    ":="

syn match    clapOperator     "&&"
syn match    clapOperator     "<"
syn match    clapOperator     ">"

syn match    clapOperator     "->"
syn match    clapOperator     "=>"
syn match    clapOperator     "<-"

syn match    clapAnyPattern   "\<_\>"
syn match    clapEmptyPattern "\<0\>"

syn match clapNumber "\<-\=\d\+\>"
syn match clapNumber "\<-\=0x\x\+\>"
syn match clapNumber "\<-\=0b[01]\+\>"

syn keyword clapPrimitive alloc read print

syn keyword clapType string char bool int unit

" Synchronization
syn sync minlines=50
syn sync maxlines=500

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_clap_syntax_inits")
  if version < 508
    let did_clap_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif


  HiLink clapCommentErr     Error
  HiLink clapThenErr        Error
  HiLink clapCharErr        Error
  HiLink clapErr            Error

  HiLink clapComment        Comment
  HiLink clapLineComment    Comment

  HiLink clapIdentifierLC   Identifier
  HiLink clapIdentifierHC   Identifier

  HiLink clapAt             Keyword
  HiLink clapKeyword        Keyword
  HiLink clapRefAssign      Keyword
  HiLink ocamlOperator      Keyword

  HiLink clapBoolean        Boolean
  HiLink clapCharacter      Character
  HiLink clapNumber         Number
  HiLink clapString         String

  HiLink clapType           Type
  HiLink clapAnyPattern     Keyword

  HiLink clapTodo           Todo

  delcommand HiLink
endif
 
let b:current_syntax = "clap"

" vim: ts=2
