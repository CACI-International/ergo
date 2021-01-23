" Vim filetype plugin file
" Language:             Ergo

if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1
let s:keepcpo= &cpo
set cpo&vim

setlocal comments=:##,:#
setlocal commentstring=#\ %s
setlocal formatoptions-=t formatoptions+=croqnl
" Newer option `j` may not be present
silent! setlocal formatoptions+=j

setlocal suffixesadd=.ergo

setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab
setlocal textwidth=100

let &cpo = s:keepcpo
unlet s:keepcpo
