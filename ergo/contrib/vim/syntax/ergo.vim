" Vim syntax file for ergo
" Language: Ergo

if exists("b:current_syntax")
	finish
endif

set iskeyword=$,%,&,',*,+,45-57,63-90,95-122,126,~
syn match ergoMerge /\^/
syn match ergoForce /!/
syn match ergoFunc /->/
syn match ergoBind /=/
syn match ergoSugar /\(|>\?\|<|\)/
syn match ergoTreeComment /#/
syn match ergoAttribute /##/

syn match ergoGetSet /:/

syn match ergoIndex contains=ergoIndexOperator,ergoBuiltins /\k\+\(:\k\+\)*/
syn match ergoPipeIndex contains=ergoIndexOperator,ergoSugar /|>\(:\k\+\)\+/
syn match ergoIndexPipe contains=ergoIndexOperator,ergoBuiltins /\(\k\+:\)\+\ze<|/
syn match ergoIndexOperator contained /:/

syn match ergoNoArgCommandOperator /:\_s/

syn match ergoDataDelimiter /[\[\]{}]/
syn region ergoQuotedString contains=ergoStringEscape,ergoStringEscapeError,ergoMerge start=/\(\\\)\@<!"/ skip=/\\[\\"]/ end=/"/
syn match ergoStringEscapeError display contained /\\\(u{.*}\|.\)/
syn match ergoStringEscape contained /\\\([\\"nt]\|u{\x\{1,6}}\)/
syn match ergoBlockString contains=ergoMerge /'\( .*\|\)$/
syn match ergoLineComment contains=ergoTodo /#\( .*\|\)$/
syn match ergoDocComment contains=ergoTodo,ergoMerge /##\( .*\|\)$/
syn match ergoBuiltins contained /\<\(ergo\|pat\|fn\|index\|std\|workspace\|doc\|bind\|unset\)\>/
syn keyword ergoTodo contained TODO FIXME XXX

let b:current_syntax = "ergo"

hi def link ergoLineComment Comment
hi def link ergoTreeComment Comment
hi def link ergoDocComment SpecialComment
hi def link ergoAttribute SpecialComment
hi def link ergoTodo Todo
hi def link ergoQuotedString String
hi def link ergoStringEscape Special
hi def link ergoStringEscapeError Error
hi def link ergoBlockString String
hi def link ergoDataDelimiter Normal
hi def link ergoNoArgCommandOperator Normal
hi def link ergoGetSet Special
hi def link ergoMerge Special
hi def link ergoForce Operator
hi def link ergoFunc Operator
hi def link ergoBind Operator
hi def link ergoSugar Macro
hi def link ergoIndexOperator Operator
hi def link ergoBuiltins Function
