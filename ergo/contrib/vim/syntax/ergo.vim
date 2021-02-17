" Vim syntax file for ergo
" Language: Ergo

if exists("b:current_syntax")
	finish
endif

set iskeyword=$,%,&,',*,+,45-57,63-90,95-122,126,~
syn keyword ergoFunctions ergo pat fn index std workspace doc
syn keyword ergoConditional if
syn match ergoGetSet /:/
syn match ergoMerge /\^/
syn match ergoForce /!/
syn match ergoFunc /->/
syn match ergoBind /=/
syn match ergoSugar /\(|>\?\|<|\)/
syn match ergoNoArgCommand contains=ergoNoArgCommandOperator /\k:/
syn match ergoNoArgCommandOperator contained /:/
syn match ergoIndex contains=ergoIndexOperator /\k:\k/
syn match ergoPipeIndex contains=ergoIndexOperator /|>:\k/
syn match ergoIndexPipe contains=ergoIndexOperator /\k:<|/
syn match ergoIndexOperator contained /:/
syn match ergoDataDelimiter /[\[\]{}]/
syn region ergoQuotedString start=/\(\\\)\@<!"/ skip=/\\[\\"]/ end=/"/
syn match ergoComment contains=ergoDocComment /#.*$/
syn match ergoDocComment /##.*$/

let b:current_syntax = "ergo"

hi def link ergoComment Comment
hi def link ergoDocComment SpecialComment
hi def link ergoQuotedString String
hi def link ergoDataDelimiter Normal
hi def link ergoNoArgCommandOperator Normal
hi def link ergoGetSet Special
hi def link ergoMerge Special
hi def link ergoForce Operator
hi def link ergoFunc Operator
hi def link ergoBind Operator
hi def link ergoSugar Macro
hi def link ergoIndexOperator Operator
hi def link ergoFunctions Function
hi def link ergoConditional Conditional
