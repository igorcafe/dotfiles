let mapleader=" "
map <Space> <Leader><Leader>
set hidden
set updatetime=300
set nobackup
set nowritebackup
set shortmess+=c
set signcolumn=number
set termguicolors
" set cmdheight=2
set updatetime=300
set hlsearch
set relativenumber
set incsearch
set tabstop=2
set shiftwidth=2
set wildmode=longest:full,full
set cc=80,120
set expandtab
set backupdir=~/.cache/nivm
set scrolloff=999
" set spell
set autoindent
" set autochdir
autocmd BufWritePre * :%s/\s\+$//e
autocmd TermOpen * startinsert
autocmd TermOpen * setlocal nonumber norelativenumber

" terminal
" nnoremap <M-t> :bel split \| resize 12 \| term<CR>
vnoremap y "+y
nnoremap p "+p
nnoremap yy "+yy
nnoremap ZA :qa!<CR>
nnoremap ZW :w<CR>
tnoremap <C-Space> <C-\><C-n>

" saving
map! <silent> <M-s> <Esc>:w<CR>
map <silent> <M-s> <Esc>:w<CR>

nnoremap ç 0
nnoremap Ç $

" adding lines
nnoremap <M-CR> o
inoremap <M-CR> <C-o>o

" delete forward/backward
inoremap <M-x> <Backspace>
cnoremap <M-x> <Backspace>
inoremap <M-d> <C-o>de
inoremap <C-d> <Delete>
cnoremap <M-d> <Space><Delete><C-Right><C-w><Backspace>
cnoremap <C-d> <Delete>

" undo
inoremap <C-_> <C-o>u

" jump to next/previous
inoremap <M-f> <C-Right>
inoremap <M-b> <C-Left>

inoremap <C-h> <Left>
inoremap <C-l> <Right>
inoremap <C-k> <Up>
inoremap <C-j> <Down>

cnoremap <M-f> <C-Right>
cnoremap <M-b> <C-Left>
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>

inoremap <C-y> <C-o>p

" jump to end/begin of line
inoremap <C-a> <Home>
inoremap <C-e> <End>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>


" noice
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

nnoremap <silent> <C-c> :noh<CR>
map <C-Space> <Esc>
map <C-Space> <Esc>
" tabs
" nmap <C-Tab> gt<CR>
" nmap <C-S-Tab> gT<CR>
nmap <M-1> 1gt<CR>
nmap <M-2> 2gt<CR>
nmap <M-3> 3gt<CR>
nmap <M-4> 4gt<CR>
nmap <M-5> 5gt<CR>
nmap <M-6> 6gt<CR>
nmap <M-7> 7gt<CR>
nmap <M-8> 8gt<CR>
nmap <M-9> 9gt<CR>
nmap <M-0> 0gt<CR>

" fix weird 'o' behavior
" nmap o o<Esc>A
" nmap O O<Esc>A

" close all files
nnoremap ZO :so $MYVIMRC<CR>:noh<CR>

" move lines
inoremap <M-j> <Esc>:m .+1<CR>gi
inoremap <M-k> <Esc>:m .-2<CR>gi
" nnoremap <A-j> :m .+1<CR>
" nnoremap <A-k> :m .-2<CR>
vnoremap <M-j> :m '>+1<CR>gv
vnoremap <M-k> :m '<-2<CR>gv

" move panels
nnoremap <M-j> <C-w>J
nnoremap <M-k> <C-w>K
nnoremap <M-h> <C-w>H
nnoremap <M-l> <C-w>L

" move between panels
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l


" Plugins
call plug#begin()
Plug 'joshdick/onedark.vim'
Plug 'vim-airline/vim-airline'
Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'preservim/tagbar'
Plug 'tpope/vim-surround'
Plug 'easymotion/vim-easymotion'
" Plug 'ctrlpvim/ctrlp.vim'
Plug 'pangloss/vim-javascript'
" Plug 'jelera/vim-javascript-syntax'
" Plug 'vim-python/python-syntax'
Plug 'neoclide/coc.nvim'
Plug 'voldikss/vim-floaterm'
" Plug 'pineapplegiant/spaceduck'
Plug 'leafOfTree/vim-vue-plugin'
Plug 'nvim-treesitter/nvim-treesitter'
" Plug 'folke/which-key.nvim'
Plug 'christianchiarulli/nvcode-color-schemes.vim'
" Plug 'nvim-lua/plenary.nvim'
" Plug 'telescope-fzf-native.nvim'
" Plug 'nvim-telescope/telescope.nvim'
" Plug 'tpope/vim-fugitive'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
" Plug 'mg979/vim-visual-multi'
call plug#end()


if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
  hi LineNr ctermbg=NONE guibg=NONE
endif

" colorscheme spaceduck
let g:nvcode_termcolors=256
colorscheme palenight

filetype plugin on


" easymotion
" map  <Leader><Leader>f <Plug>(easymotion-bd-f)
" nmap <Leader><Leader>F <Plug>(easymotion-overwin-f)
" nmap s <Plug>(easymotion-overwin-f2)
" map <Leader><Leader>l <Plug>(easymotion-bd-jk)
" nmap <Leader><Leader>L <Plug>(easymotion-overwin-line)
" map  <Leader><Leader>w <Plug>(easymotion-bd-w)
" nmap <Leader><Leader>W <Plug>(easymotion-overwin-w)

" ctrlp
let g:ctrlp_user_command = 'fd -t f "" %s'

source ~/.config/nvim/settings__nerdcommenter.vim
source ~/.config/nvim/settings__nerdtree.vim
source ~/.config/nvim/settings__floaterm.vim
source ~/.config/nvim/settings__coc.vim
source ~/.config/nvim/settings__vue.vim

nmap <M-f><M-f> <cmd>Files<cr>
nmap <M-f><M-g> <cmd>Rg<cr>
nmap <M-f><M-b> <cmd>Buffers<cr>

lua << EOF
  require'nvim-treesitter.configs'.setup {
    ensure_installed = "all",

    highlight = {
      enable = true,
      disable = { "vim" }
    },
  }

EOF


