nmap <F8> :TagbarToggle<CR>
nnoremap <F2> :NERDTreeToggle<CR>
"nnoremap <C-f> :NERDTreeFind<CR>
"autocmd VimEnter * NERDTree | wincmd p
autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif
