tnoremap <Esc> <C-\><C-n>
au BufEnter * if &buftype == 'terminal' | :startinsert | endif
function! OpenTerminal()
	split term://zsh
	resize 12
endfunction
nnoremap <M-t> :call OpenTerminal()<CR>
