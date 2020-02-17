" Enable python support
" :let &pythonthreedll = '..\vimfiles\python36.dll'

" manage plugins using plug.vim  https://github.com/junegunn/vim-plug
call plug#begin('../plugged')

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
"Plug 'mattn/emmet-vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'justinmk/vim-sneak'
Plug 'MattesGroeger/vim-bookmarks'
Plug 'sheerun/vim-polyglot'
Plug 'chaoren/vim-wordmotion'
Plug 'altercation/vim-colors-solarized'
"Plug 'terryma/vim-multiple-cursors' " This is complete garbage but havn't found a better alternative
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'dir': '../fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
" Plug 'rafaqz/ranger.vim' " does not work well on windows, can amange with fzf working
" Plug 'liuchengxu/vim-which-key' - works sometimes, probably needs more configuration
"Plug 'lifepillar/vim-mucomplete'
"Plug 'ycm-core/YouCompleteMe'

"This is reuired for snippets with snipmate
"Plug 'MarcWeber/vim-addon-mw-utils'
"Plug 'tomtom/tlib_vim'
"Plug 'garbas/vim-snipmate'

" Track the engine.
"Plug 'SirVer/ultisnips'

" This contains snippets
" Plug 'honza/vim-snippets'

call plug#end()
" vim-sneak mappings
"map s <Plug> Sneak_t
"map S <Plug> Sneak_T


" Trigger configuration for ultisnip snippets. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
"let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsJumpForwardTrigger="<c-b>"
"let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Mappings for snipmate
":imap <Tab> <Plug>snipMateNextOrTrigger

" plugin configurations
let g:airline_theme='jellybeans'
"let g:user_emmet_mode='a'
"let g:user_emmet_leader_key=' '

" set ranger to use externam terminal because of bug using gvim
"let g:ranger_terminal = 'urxvt -e'
"let g:ranger_terminal = 'xterm -e'

" Manage language support for custom extensions
autocmd BufNewFile,BufRead *.luascn set syntax=lua
autocmd BufNewFile,BufRead *.luascn set nospell
autocmd BufNewFile,BufRead *.lua set nospell

" basic settings - https://www.shortcutfoo.com/blog/top-50-vim-configuration-options/

" search settings
set ignorecase
set smartcase
set incsearch
set hlsearch

" indentation settings
set autoindent
set tabstop=4 shiftwidth=0
"set softabstop=4 shiftwidth=4

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
"nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
"vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>

"Manging session
nnoremap <leader>qq :q!<CR>

"Managing file explorer
map <leader>rr :RangerEdit<cr> " Browse and open files
map <leader>rv :RangerVSplit<cr> " Browse veritcal split
map <leader>rs :RangerSplit<cr> " Browse in splits
map <leader>rt :RangerTab<cr> " Browse in tabs
map <leader>ri :RangerInsert<cr> " Insert file path
map <leader>ra :RangerAppend<cr> " Append file path
map <leader>rc :set operatorfunc=RangerChangeOperator<cr>g@ " Change file path under cursor
map <leader>rd :RangerCD<cr> " Change current working directory
map <leader>rld :RangerLCD<cr> " Change current working directory

"Managing files
nnoremap <leader>fs :w<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fvd : cd

"Managing buffers
nnoremap <leader>bb :Buffers<CR>
nnoremap <leader>bd :bd<CR>
nnoremap <leader><tab> :bl<CR>

"Managing tabs
nnoremap <leader>ta :tabnew<CR>
nnoremap <leader>tx :tabc<CR>
nnoremap <leader>tX :tabc!<CR>
nnoremap <leader>t^ :tabfir<CR>
nnoremap <leader>t$ :tablast<CR>
nnoremap <leader>th :tabmove -1<CR>
nnoremap <leader>tl :tabmove +1<CR>

"Managing searches
nnoremap <leader>sc :noh<CR>

"Manage version control - Requires fugitive to be set up
" nnoremap <leader>gc :Commits<CR>

" maps the window splitting and resizing
nnoremap <leader>h <C-w>h
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>l <C-w>l
nnoremap <leader>wd <C-w>wq " This needs to be tested
nnoremap <leader>w <C-w>

" Managing toggles
nnoremap <leader>tn :set number!<CR>
nnoremap <leader>tr :set relativenumber!<CR>

" Manage bookmarks
nnoremap <leader>fj <Cmd>call fzf#run(fzf#wrap({
	\ 'source': readfile('../bookmarks'),
	\ 'options': [
		\ '--multi',
	\ ],
\ }))<CR>
nnoremap <leader>fe :e ../bookmarks<CR>


"Misc
" remove trailing whitespace on save
" autocmd BufWritePre *.lua,*.luascn %s/\s\+$

nnoremap <leader>ig :noraml! "oyiWdd"pyiWdd"gpwwdE"oPjj^f/;wdt""oPj^fMwwdt;"pPjjjj<CR>
nnoremap <leader>im :noraml! "oyiWdd"npwwdt "oPf/wdt""oPyiW^wwyt<CR>
nnoremap <leader>gtd :cd G:\fd\Applications\Mining\VRSM\Data<CR>