" Vim syntax file for ergo
" Language: Ergo

if exists("b:current_syntax")
	finish
endif

syn keyword ergoKeys fn match
syn match ergoIndex /:/
syn match ergoMerge /\^/
syn match ergoSugar /\(|>\?\|<|\)/
syn match ergoDataDelimiter /[\[\]{}]/
syn region ergoQuotedString start=/\(\\\)\@<!"/ skip=/\\[\\"]/ end=/"/
syn match ergoComment /#.*$/

let b:current_syntax = "ergo"

hi def link ergoComment Comment
hi def link ergoQuotedString String
hi def link ergoDataDelimiter Normal
hi def link ergoIndex Type
hi def link ergoMerge Type
hi def link ergoSugar Statement
hi def link ergoKeys Statement
