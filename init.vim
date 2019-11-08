" plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'https://github.com/majutsushi/tagbar.git'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'https://github.com/rbgrouleff/bclose.vim'
Plug 'junegunn/fzf'
Plug 'lervag/vimtex'
Plug 'kristijanhusak/vim-hybrid-material'
Plug 'honza/vim-snippets'
Plug 'tpope/vim-surround'
Plug 'junegunn/fzf.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()
"""

" editor settings
" colors
set termguicolors
set background=dark
colo hybrid_reverse
highlight ColorColumn ctermbg=darkgray
:hi CursorLine cterm=none
:hi CursorLine gui=none

" behaviour
set autoread
set hidden
set noswapfile
set mouse=a
set updatetime=300
set shortmess+=c
set splitbelow
set splitright

" text edit settings
set tabstop=4
set softtabstop=4
set shiftwidth=4
set autoindent
set smartindent
set smarttab
set expandtab
set textwidth=110

" misc
set completeopt-=preview
set nu
set relativenumber
set cursorline
set showtabline=2
autocmd BufRead,BufNewFile * setlocal signcolumn=yes
autocmd FileType tagbar,nerdtree setlocal signcolumn=no

" tidy-up whitespaces before write
autocmd BufWritePre * %s/\s\+$//e

" Make Sure that Vim returns to the same line when we reopen a file"
augroup line_return
    au!
    au BufReadPost gitcommit let b:execute_on_git_commit=true
    au BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") && !exists("b:execute_on_git_commit") |
                \ execute 'normal! g`"zvzz' |
                \ endif
augroup END
"""

" plug settings

" coc.nvim
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm() :
                                       \"\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
autocmd CursorHold * silent call CocActionAsync('highlight')
nmap <F2> <Plug>(coc-rename)
command! -nargs=0 Format :call CocAction('format')

" NERDTree
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeHijackNetrw=1
let g:NERDTreeMapJumpPrevSibling=""
let g:NERDTreeMapJumpNextSibling=""
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" UltiSnips
set runtimepath+=~/dotfiles/snippets
let g:UltiSnipsSnippetsDir="~/dotfiles/snippets/UltiSnips"

let g:UltiSnipsJumpForwardTrigger	= "<c-j>"
let g:UltiSnipsJumpBackwardTrigger	= "<c-k>"
let g:UltiSnipsRemoveSelectModeMappings = 0

" airline
let g:airline_theme = 'atomic'
let g:airline#extensions#tabline#enabled = 2
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#right_sep = ' '
let g:airline#extensions#tabline#right_alt_sep = '|'
let g:airline_left_sep = ' '
let g:airline_left_alt_sep = '|'
let g:airline_right_sep = ' '
let g:airline_right_alt_sep = '|'
"""

" key mappings
" disable arrows
inoremap <Down> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>
inoremap <Up> <Nop>

nnoremap <Down> <Nop>
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>
nnoremap <Up> <Nop>

vnoremap <Down> <Nop>
vnoremap <Left> <Nop>
vnoremap <Right> <Nop>
vnoremap <Up> <Nop>
" use space as a Leader key
let mapleader = ' '

" fight tabs with tabs
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprev<CR>
nnoremap <silent> <Leader>bd :Bclose<CR>

" split movement
nnoremap <silent> <leader><Up> :wincmd k<CR>
nnoremap <silent> <leader><Down> :wincmd j<CR>
nnoremap <silent> <leader><Left> :wincmd h<CR>
nnoremap <silent> <leader><Right> :wincmd l<CR>
nnoremap <silent> <leader>k :wincmd k<CR>
nnoremap <silent> <leader>j :wincmd j<CR>
nnoremap <silent> <leader>h :wincmd h<CR>
nnoremap <silent> <leader>l :wincmd l<CR>

" config files shortcuts
nnoremap <silent> <leader>ov :e $MYVIMRC<CR>
nnoremap <silent> <leader>sv :w<CR> :so $MYVIMRC<CR>
nnoremap <silent> <leader>tv :e $HOME/.tmux.conf<CR>
nnoremap <silent> <leader>i3 :e $HOME/.config/sway/config<CR>

" sidebars
nnoremap <silent> <Leader>nn :NERDTreeToggle<CR>
nnoremap <silent> <Leader>tt :TagbarToggle<CR>

" terminal
nnoremap <silent> <leader>tm :split \| resize 17 \| term<CR>
tnoremap <silent> <Esc> <C-\><C-n>

" save the buffer!
nnoremap zs :w<CR>
"""

" source local vim settings
sil! source ~/.config/nvim/local.vim
