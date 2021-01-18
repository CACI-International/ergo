" Vim syntax file for ergo
" Language: Ergo

if exists("b:current_syntax")
	finish
endif

syn iskeyword $,%,&,',45-57,63-90,95-122,126,~
syn keyword ergoFunctions pat fn
syn keyword ergoConditional if
syn match ergoGetSet /:/
syn match ergoMerge /\^/
syn match ergoForce /!/
syn match ergoFunc /->/
syn match ergoBind /=/
syn match ergoSugar /\(|>\?\|<|\)/
syn match ergoSugarColonGroup contains=ergoSugarColon /\k\+:/
syn match ergoSugarColon contained /:/
syn match ergoDataDelimiter /[\[\]{}]/
syn region ergoQuotedString start=/\(\\\)\@<!"/ skip=/\\[\\"]/ end=/"/
syn match ergoComment contains=ergoDocComment /#.*$/
syn match ergoDocComment /## .*$/

let b:current_syntax = "ergo"

hi def link ergoComment Comment
hi def link ergoDocComment SpecialComment
hi def link ergoQuotedString String
hi def link ergoDataDelimiter Normal
hi def link ergoGetSet Operator
hi def link ergoMerge Operator
hi def link ergoForce Operator
hi def link ergoFunc Operator
hi def link ergoBind Operator
hi def link ergoSugar Macro
hi def link ergoSugarColon Macro
hi def link ergoFunctions Function
hi def link ergoConditional Conditional
