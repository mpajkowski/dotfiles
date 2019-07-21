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
Plug 'sakhnik/nvim-gdb'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'tpope/vim-surround'
Plug 'Raimondi/delimitMate'
Plug 'junegunn/fzf.vim'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-ultisnips'
Plug 'roxma/nvim-yarp'
call plug#end()
"""

" editor settings
  " colors
  set termguicolors
  set completeopt-=preview
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

  " text edit settings
  set tabstop=2
  set softtabstop=2
  set shiftwidth=2
  set autoindent
  set smartindent
  set smarttab
  set expandtab
  set textwidth=110

  " misc
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
  " ncm2
  autocmd BufEnter * call ncm2#enable_for_buffer()
  set completeopt=noinsert,menuone,noselect

  " ncm2-ultisnips
  inoremap <silent> <expr> <CR> ncm2_ultisnips#expand_or("\<CR>", 'n')

  " NERDTree
  let NERDTreeAutoDeleteBuffer = 1
  let NERDTreeHijackNetrw=1
  let g:NERDTreeMapJumpPrevSibling=""
  let g:NERDTreeMapJumpNextSibling=""

  " LanguageClient
  let g:LanguageClient_serverCommands = {
      \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
      \ 'python': ['pyls']
      \ }
  let g:LanguageClient_hasSnippetSupport = 1

  let g:LanguageClient_useVirtualText = 0

  " UltiSnips
  set runtimepath+=~/dotfiles/snippets
  let g:UltiSnipsSnippetsDir="~/dotfiles/snippets/UltiSnips"

  let g:UltiSnipsJumpForwardTrigger	= "<c-k>"
  let g:UltiSnipsJumpBackwardTrigger	= "<c-j>"
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
  " use space as a Leader key
  let mapleader = ' '

  " fight tabs with tabs
  nnoremap <Tab> :bnext<CR>
  nnoremap <S-Tab> :bprev<CR>
  nnoremap <silent> <Leader>bd :Bclose<CR>

  " quickfix
  nnoremap <silent> ]c :cn<CR>
  nnoremap <silent> [c :cp<CR>
  nnoremap <silent> <Leader>c :copen<CR>

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
  nnoremap <silent> <leader>i3 :e $HOME/.config/i3/config<CR>

  " sidebars
  nnoremap <silent> <Leader>nn :NERDTreeToggle<CR>
  nnoremap <silent> <Leader>tt :TagbarToggle<CR>

  " save the buffer!
  nnoremap zs :w<CR>
"""

" source local vim settings
sil! source ~/.config/nvim/local.vim
