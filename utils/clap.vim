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

" " lowercase identifier - the standard way to match
" syn match    ocamlLCIdentifier /\<\(\l\|_\)\(\w\|'\)*\>/
" 
" " Errors
" syn match    ocamlBraceErr   "}"
" syn match    ocamlBrackErr   "\]"
" syn match    ocamlParenErr   ")"
" syn match    ocamlArrErr     "|]"
" 
" syn match    ocamlCommentErr "\*)"
" 
" syn match    ocamlThenErr    "\<then\>"
" 
" " Some convenient clusters
" syn cluster  ocamlAllErrs contains=ocamlBraceErr,ocamlBrackErr,ocamlParenErr,ocamlCommentErr,ocamlCountErr,ocamlDoErr,ocamlDoneErr,ocamlEndErr,ocamlThenErr
" 
" syn cluster  ocamlAENoParen contains=ocamlBraceErr,ocamlBrackErr,ocamlCommentErr,ocamlCountErr,ocamlDoErr,ocamlDoneErr,ocamlEndErr,ocamlThenErr
" 
" syn cluster  ocamlContained contains=ocamlTodo,ocamlPreDef,ocamlModParam,ocamlModParam1,ocamlPreMPRestr,ocamlMPRestr,ocamlMPRestr1,ocamlMPRestr2,ocamlMPRestr3,ocamlModRHS,ocamlFuncWith,ocamlFuncStruct,ocamlModTypeRestr,ocamlModTRWith,ocamlWith,ocamlWithRest,ocamlModType,ocamlFullMod,ocamlVal
" 
" 
" " Enclosing delimiters
" syn region   ocamlEncl transparent matchgroup=ocamlKeyword start="(" matchgroup=ocamlKeyword end=")" contains=ALLBUT,@ocamlContained,ocamlParenErr
" syn region   ocamlEncl transparent matchgroup=ocamlKeyword start="{" matchgroup=ocamlKeyword end="}"  contains=ALLBUT,@ocamlContained,ocamlBraceErr
" syn region   ocamlEncl transparent matchgroup=ocamlKeyword start="\[" matchgroup=ocamlKeyword end="\]" contains=ALLBUT,@ocamlContained,ocamlBrackErr
" syn region   ocamlEncl transparent matchgroup=ocamlKeyword start="\[|" matchgroup=ocamlKeyword end="|\]" contains=ALLBUT,@ocamlContained,ocamlArrErr
" 
" 
" Comments
syn region   clapComment start="(\*" end="\*)" contains=clapComment,clapTodo
syn region   clapLineComment start="\*\*" end="$" oneline contains=clapTodo
syn keyword  clapTodo contained TODO FIXME XXX NOTE
" 
" 
" " Objects
" syn region   ocamlEnd matchgroup=ocamlObject start="\<object\>" matchgroup=ocamlObject end="\<end\>" contains=ALLBUT,@ocamlContained,ocamlEndErr
" 
" 
" " Blocks
" if !exists("ocaml_revised")
"   syn region   ocamlEnd matchgroup=ocamlKeyword start="\<begin\>" matchgroup=ocamlKeyword end="\<end\>" contains=ALLBUT,@ocamlContained,ocamlEndErr
" endif
" 
" 
" " "for"
" syn region   ocamlNone matchgroup=ocamlKeyword start="\<for\>" matchgroup=ocamlKeyword end="\<\(to\|downto\)\>" contains=ALLBUT,@ocamlContained,ocamlCountErr
" 
" 
" " "do"
" if !exists("ocaml_revised")
"   syn region   ocamlDo matchgroup=ocamlKeyword start="\<do\>" matchgroup=ocamlKeyword end="\<done\>" contains=ALLBUT,@ocamlContained,ocamlDoneErr
" endif
" 
" " "if"
" syn region   ocamlNone matchgroup=ocamlKeyword start="\<if\>" matchgroup=ocamlKeyword end="\<then\>" contains=ALLBUT,@ocamlContained,ocamlThenErr
" 
" 
" " "struct"
" syn region   ocamlStruct matchgroup=ocamlModule start="\<\(module\s\+\)\=struct\>" matchgroup=ocamlModule end="\<end\>" contains=ALLBUT,@ocamlContained,ocamlEndErr
" 
" " "module type"
" syn region   ocamlKeyword start="\<module\>\s*\<type\>\(\s*\<of\>\)\=" matchgroup=ocamlModule end="\<\w\(\w\|'\)*\>" contains=ocamlComment skipwhite skipempty nextgroup=ocamlMTDef
" syn match    ocamlMTDef "=\s*\w\(\w\|'\)*\>"hs=s+1,me=s
" 
" syn keyword  ocamlKeyword  and as assert class
" syn keyword  ocamlKeyword  constraint else
" syn keyword  ocamlKeyword  exception external fun
" 
" syn keyword  ocamlKeyword  in inherit initializer
" syn keyword  ocamlKeyword  land lazy let match
" syn keyword  ocamlKeyword  method mutable new of
" syn keyword  ocamlKeyword  parser private raise rec
" syn keyword  ocamlKeyword  try type
" syn keyword  ocamlKeyword  virtual when while with
" 
" if exists("ocaml_revised")
"   syn keyword  ocamlKeyword  do value
"   syn keyword  ocamlBoolean  True False
" else
"   syn keyword  ocamlKeyword  function
"   syn keyword  ocamlBoolean  true false
"   syn match    ocamlKeyChar  "!"
" endif
" 
" syn keyword  ocamlType     array bool char exn float format format4
" syn keyword  ocamlType     int int32 int64 lazy_t list nativeint option
" syn keyword  ocamlType     string unit
" 
" syn keyword  ocamlOperator asr lnot lor lsl lsr lxor mod not
" 
" syn match    ocamlConstructor  "(\s*)"
" syn match    ocamlConstructor  "\[\s*\]"
" syn match    ocamlConstructor  "\[|\s*>|]"
" syn match    ocamlConstructor  "\[<\s*>\]"
" syn match    ocamlConstructor  "\u\(\w\|'\)*\>"
" 
" " Polymorphic variants
" syn match    ocamlConstructor  "`\w\(\w\|'\)*\>"
" 
" " Module prefix
" syn match    ocamlModPath      "\u\(\w\|'\)*\."he=e-1
" 
" syn match    ocamlCharacter    "'\\\d\d\d'\|'\\[\'ntbr]'\|'.'"
" syn match    ocamlCharacter    "'\\x\x\x'"
" syn match    ocamlCharErr      "'\\\d\d'\|'\\\d'"
" syn match    ocamlCharErr      "'\\[^\'ntbr]'"
" syn region   ocamlString       start=+"+ skip=+\\\\\|\\"+ end=+"+
" 
" syn match    ocamlFunDef       "->"
" syn match    ocamlRefAssign    ":="
" syn match    ocamlTopStop      ";;"
" syn match    ocamlOperator     "\^"
" 
" syn match    ocamlOperator     "&&"
" syn match    ocamlOperator     "<"
" syn match    ocamlOperator     ">"
" syn match    ocamlAnyVar       "\<_\>"
" syn match    ocamlKeyChar      "|[^\]]"me=e-1
" syn match    ocamlKeyChar      ";"
" syn match    ocamlKeyChar      "\~"
" syn match    ocamlKeyChar      "?"
" syn match    ocamlKeyChar      "\*"
" syn match    ocamlKeyChar      "="
" 
" if exists("ocaml_revised")
"   syn match    ocamlErr        "<-"
" else
"   syn match    ocamlOperator   "<-"
" endif
" 
" syn match    ocamlNumber        "\<-\=\d\(_\|\d\)*[l|L|n]\?\>"
" syn match    ocamlNumber        "\<-\=0[x|X]\(\x\|_\)\+[l|L|n]\?\>"
" syn match    ocamlNumber        "\<-\=0[o|O]\(\o\|_\)\+[l|L|n]\?\>"
" syn match    ocamlNumber        "\<-\=0[b|B]\([01]\|_\)\+[l|L|n]\?\>"
" syn match    ocamlFloat         "\<-\=\d\(_\|\d\)*\.\?\(_\|\d\)*\([eE][-+]\=\d\(_\|\d\)*\)\=\>"
" 
" " Labels
" syn match    ocamlLabel        "\~\(\l\|_\)\(\w\|'\)*"lc=1
" syn match    ocamlLabel        "?\(\l\|_\)\(\w\|'\)*"lc=1
" syn region   ocamlLabel transparent matchgroup=ocamlLabel start="?(\(\l\|_\)\(\w\|'\)*"lc=2 end=")"me=e-1 contains=ALLBUT,@ocamlContained,ocamlParenErr
" 
" 
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
" 
"   HiLink ocamlBraceErr       Error
"   HiLink ocamlBrackErr       Error
"   HiLink ocamlParenErr       Error
"   HiLink ocamlArrErr       Error
" 
"   HiLink ocamlCommentErr   Error
" 
"   HiLink ocamlCountErr       Error
"   HiLink ocamlDoErr       Error
"   HiLink ocamlDoneErr       Error
"   HiLink ocamlEndErr       Error
"   HiLink ocamlThenErr       Error
" 
"   HiLink ocamlCharErr       Error
" 
"   HiLink ocamlErr       Error
" 
    HiLink clapComment      Comment
    HiLink clapLineComment    Comment

"   HiLink ocamlModPath       Include
"   HiLink ocamlObject       Include
"   HiLink ocamlModule       Include
"   HiLink ocamlModParam1    Include
"   HiLink ocamlModType       Include
"   HiLink ocamlMPRestr3       Include
"   HiLink ocamlFullMod       Include
"   HiLink ocamlModTypeRestr Include
"   HiLink ocamlWith       Include
"   HiLink ocamlMTDef       Include
" 
"   HiLink ocamlScript       Include
" 
"   HiLink ocamlConstructor  Constant
" 
"   HiLink ocamlVal          Keyword
"   HiLink ocamlModPreRHS    Keyword
"   HiLink ocamlMPRestr2       Keyword
"   HiLink ocamlKeyword       Keyword
"   HiLink ocamlMethod       Include
"   HiLink ocamlFunDef       Keyword
"   HiLink ocamlRefAssign    Keyword
"   HiLink ocamlKeyChar       Keyword
"   HiLink ocamlAnyVar       Keyword
"   HiLink ocamlTopStop       Keyword
"   HiLink ocamlOperator       Keyword
" 
"   HiLink ocamlBoolean       Boolean
"   HiLink ocamlCharacter    Character
"   HiLink ocamlNumber       Number
"   HiLink ocamlFloat       Float
"   HiLink ocamlString       String
" 
"   HiLink ocamlLabel       Identifier
" 
"   HiLink ocamlType       Type
" 
    HiLink ocamlTodo    Todo
" 
"   HiLink ocamlEncl       Keyword
" 
    delcommand HiLink
endif
 
let b:current_syntax = "clap"
" 
" vim: ts=4
