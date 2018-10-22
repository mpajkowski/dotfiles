" plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'https://github.com/majutsushi/tagbar.git'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'https://github.com/rbgrouleff/bclose.vim'
Plug 'junegunn/fzf'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'jiangmiao/auto-pairs'
Plug 'lervag/vimtex'
Plug 'kristijanhusak/vim-hybrid-material'
Plug 'rhysd/vim-clang-format'
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-path'
Plug 'sakhnik/nvim-gdb'
call plug#end()

autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'c': ['ccls', '--log-file=/tmp/cc.log'],
    \ 'cpp': ['ccls', '--log-file=/tmp/cc.log'],
    \ 'cuda': ['ccls', '--log-file=/tmp/cc.log'],
    \ 'objc': ['ccls', '--log-file=/tmp/cc.log'],
    \ }

let g:LanguageClient_autoStart = 1
set formatexpr=LanguageClient_textDocument_rangeFormatting()

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>

colorscheme hybrid_reverse
set background=dark
set hidden
set noswapfile
set nobackup
set nu
set mouse=a
set tabstop=2
set softtabstop=2
set shiftwidth=2
set autoindent
set smartindent
set smarttab
set expandtab
set cursorline
set showtabline=2
set textwidth=110
set termguicolors
set relativenumber

highlight ColorColumn ctermbg=darkgray
:hi CursorLine cterm=none
:hi CursorLine gui=none

autocmd BufRead,BufNewFile * setlocal signcolumn=yes
autocmd FileType tagbar,nerdtree setlocal signcolumn=no

let mapleader = ' '
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprev<CR>
nnoremap <silent> <leader><Up> :wincmd k<CR>
nnoremap <silent> <leader><Down> :wincmd j<CR>
nnoremap <silent> <leader><Left> :wincmd h<CR>
nnoremap <silent> <leader><Right> :wincmd l<CR>
nnoremap <silent> <leader>k :wincmd k<CR>
nnoremap <silent> <leader>j :wincmd j<CR>
nnoremap <silent> <leader>h :wincmd h<CR>
nnoremap <silent> <leader>l :wincmd l<CR>
nnoremap <silent> <leader>ov :e $MYVIMRC<CR>
nnoremap <silent> <leader>sv :w<CR> :so $MYVIMRC<CR>
nnoremap <silent> <leader>tv :e $HOME/.tmux.conf<CR>
nnoremap <silent> <leader>i3 :e $HOME/.config/i3/config<CR>
nnoremap zs :w<CR>

" Make Sure that Vim returns to the same line when we reopen a file"
augroup line_return
    au!
    au BufReadPost *
                \ if line("'\"") > 0 && line("'\"") <= line("$") |
                \ execute 'normal! g`"zvzz' |
                \ endif
augroup END

let g:airline_theme = "atomic"
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

" Leader bindings
nnoremap <silent> <Leader>nn :NERDTreeToggle<CR>
nnoremap <silent> <Leader>tt :TagbarToggle<CR>
nnoremap <silent> <Leader>bd :Bclose<CR>
nnoremap <silent> <Leader>qq :q<CR>

nnoremap <silent> <Leader>" viw<esc>a"<esc>bi"<esc>
nnoremap <silent> <Leader>' viw<esc>a'<esc>bi'<esc>
nnoremap <silent> <Leader>( viw<esc>a)<esc>bi(<esc>
nnoremap <silent> <Leader>) viw<esc>a)<esc>bi(<esc>
nnoremap <silent> <Leader>[ viw<esc>a]<esc>bi[<esc>
nnoremap <silent> <Leader>] viw<esc>a]<esc>bi[<esc>
nnoremap <silent> <Leader>{ viw<esc>a}<esc>bi{<esc>
nnoremap <silent> <Leader>} viw<esc>a}<esc>bi{<esc>

autocmd BufWritePre * %s/\s\+$//e
autocmd BufWritePre *.h,*.hpp,*.c,*.cpp,*.cc ClangFormat
