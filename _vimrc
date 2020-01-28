" manage plugins using plug.vim  https://github.com/junegunn/vim-plug
call plug#begin('../plugged')

Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'MattesGroeger/vim-bookmarks'
Plug 'sheerun/vim-polyglot'
Plug 'chaoren/vim-wordmotion'
Plug 'altercation/vim-colors-solarized'
Plug 'terryma/vim-multiple-cursors' " This is complete garbage but havn't found a better alternative
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'dir': '../fzf', 'do': './install --all' }

" This should be added when there is time https://vimawesome.com/plugin/youcompleteme

"This is reuired for snippets with snipmate
"Plug 'MarcWeber/vim-addon-mw-utils'
"Plug 'tomtom/tlib_vim'
"Plug 'garbas/vim-snipmate'

" Track the engine.
"Plug 'SirVer/ultisnips'

" This contains snippets
Plug 'honza/vim-snippets'

call plug#end()

" Trigger configuration for ultisnip snippets. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
"let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsJumpForwardTrigger="<c-b>"
"let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" plugin configurations
let g:airline_theme='jellybeans'

" basic settings - https://www.shortcutfoo.com/blog/top-50-vim-configuration-options/

" search settings
set smartcase
set incsearch
set hlsearch

" indentation settings
set autoindent

" text rendering
syntax enable
set background=dark
colorscheme solarized
set guifont=Hack
set encoding=utf-8

" editing
set foldmethod=syntax
set backspace=indent,eol,start

" interface options
set number
set relativenumber

" store swap files
" set directory='../swap' this does not work but i want it to

"hide files that are not open instead of closing them
set hidden

"spell checking
set spell

"set the shell used
" happy with default, but good to remember
" set shell{PATH}


" keymaps

let mapleader = " "

nnoremap <leader>qq :q!<CR>
nnoremap <leader>fs :w<CR>

" maps the window splitting and resizing
nnoremap <leader>h <C-w>h
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>l <C-w>l
nnoremap <leader>w <C-w>

" convenient toggles
" line numbers
nnoremap <leader>tn :set number!<CR>
nnoremap <leader>tr :set relativenumber!<CR>