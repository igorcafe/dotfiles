call plug#begin('~/.vim/plugged')
Plug 'sheerun/vim-polyglot'
Plug 'jiangmiao/auto-pairs'
Plug 'preservim/tagbar'
Plug 'preservim/nerdtree'
Plug 'joshdick/onedark.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-syntastic/syntastic'
Plug 'ryanoasis/vim-devicons'
Plug 'airblade/vim-gitgutter'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

colorscheme onedark
set encoding=UTF-8
set nocompatible
set number
set ruler
set showcmd
set incsearch
set hlsearch
set hidden
set ttimeoutlen=20
set scrolloff=999
set complete-=i
set wildmode=longest:full,full
set whichwrap+=<,>,h,l,[,]


" fix weird behavior
nmap o o<Esc>i
nmap O O<Esc>i

nmap ZA :qa!<CR>

source ~/.vim/cocsettings.vim
source ~/.vim/nerdtreesettings.vim

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

