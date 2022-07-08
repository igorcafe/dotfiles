set autochdir
syntax enable
colorscheme dracula
set termguicolors
set number
set splitright
set splitbelow
set complete-=i
set wildmode=longest:full,full
set ttimeoutlen=20
set scrolloff=999
set autoindent
set whichwrap+=<,>,h,l,[,]
set linebreak
set nolist
set tabstop=4
set shiftwidth=4
set expandtab

" Do not show line numbers in terminal
autocmd TermOpen * setlocal nonumber norelativenumber

" Alt + 3 for toggling relative line numbers
nmap <M-3> :set relativenumber!<CR>

" fix weird behavior
nmap o o<Esc>i
nmap O O<Esc>i

" Move lines up and down
map <M-k> ddkP
map <M-j> ddjP

" ctrl space to autocomplete
inoremap <C-Space> <c-n>

