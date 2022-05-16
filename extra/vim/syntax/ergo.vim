" Vim syntax file for ergo
" Language: Ergo

if exists("b:current_syntax")
	finish
endif

set iskeyword=!,%,&,',*,+,45-57,63-90,95-122

let b:current_syntax = "ergo"

hi link CocSemDocumentationComment SpecialComment
hi link CocSemVariable PreProc
hi link CocSemDefaultLibraryVariable Function
