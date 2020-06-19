" TODO : Investigate https://github.com/freitass/todo.txt-vim vs https://github.com/jceb/vim-orgmode


" Enable python support
" :let &pythonthreedll = '..\vimfiles\python36.dll'

" manage plugins using plug.vim  https://github.com/junegunn/vim-plug
call plug#begin()

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'justinmk/vim-sneak'
Plug 'vimwiki/vimwiki'
"Plug 'MattesGroeger/vim-bookmarks'
Plug 'mbbill/undotree'
Plug 'sheerun/vim-polyglot'
Plug 'chaoren/vim-wordmotion'
Plug 'altercation/vim-colors-solarized'
Plug 'nanotech/jellybeans.vim'
"Plug 'terryma/vim-multiple-cursors' " This is complete garbage but havn't found a better alternative
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/fzf', { 'dir': '../fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
" Plug 'lifepillar/vim-mucomplete'
Plug 'wsdjeg/FlyGrep.vim'
Plug 'drmikehenry/vim-fontsize'

if has('nvim') || has('patch-8.0.902') "Requires async support
  Plug 'mhinz/vim-signify'
else
  Plug 'mhinz/vim-signify', { 'branch': 'legacy' }
endif

" Plug 'rafaqz/ranger.vim' " does not work well on windows, can manage with fzf working
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

Plug 'vim-scripts/vcscommand.vim'

call plug#end()
" vim-sneak mappings
"map s <Plug> Sneak_t
"map S <Plug> Sneak_T

" vim-wiki config
" let wiki_settings={
 \ 'template_path': vimwiki_export_path',
 \ 'template_default': 'default',
 \ 'template_ext': '.html',
 \ 'auto_export': 0,
 \ 'nested_syntaxes': {
 \ 'js':'javascript',
 \ }}

"Auto complete config for mucomplete
" let g:mucomplete#enable_auto_at_startup = 1
" let g:mucomplete#minimum_prefix_length = 3
" let g:mucomplete#buffer_relative_paths = 1
" set completeopt=menuone,noselect,noinsert shortmess+=c
" inoremap <expr> <CR> pumvisible() ? "\<C-Y>\<CR>" : "\<CR>"

"Limelight use with goyo
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

" Disables sound from constant errors while typing - I would like to solve the
" errors, but I'm not sure how
autocmd GUIEnter * set vb t_vb=


" Trigger configuration for ultisnip snippets. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
"let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsJumpForwardTrigger="<c-b>"
"let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Mappings for snipmate
":imap <Tab> <Plug>snipMateNextOrTrigger

" plugin configurations
let g:airline_theme='jellybeans'
"let g:user_emmet_mode='a'
"let g:user_emmet_leader_key='<Leader>'

let g:signify_disable_by_default = 0
let g:signify_line_highlight = 0
nnoremap <Leader>fvt :SignifyToggle<CR>
nnoremap <Leader>fvd :SignifyDiff<CR>
nnoremap <Leader>fvh :SignifyToggleHighlight<CR>

" set ranger to use externam terminal because of bug using gvim
"let g:ranger_terminal = 'urxvt -e'
"let g:ranger_terminal = 'xterm -e'

" Manage language support for custom extensions
autocmd BufNewFile,BufRead *.luascn set syntax=lua
autocmd BufNewFile,BufRead *.luascn set nospell
autocmd BufNewFile,BufRead *.luascn setlocal commentstring=--\ %s
autocmd BufNewFile,BufRead *.smeta set syntax=lua
autocmd BufNewFile,BufRead *.smeta set nospell
autocmd BufNewFile,BufRead *.smeta setlocal commentstring=--\ %s
autocmd BufNewFile,BufRead *.lua set nospell

" basic settings - https://www.shortcutfoo.com/blog/top-50-vim-configuration-options/

" search settings
set ignorecase
set smartcase
set incsearch
set hlsearch
nnoremap <leader>fs /<C-r>0 'TODO: Need a keybinding that doesn't interfere with seek

" indentation settings
set autoindent
set tabstop=4 shiftwidth=0

"set softabstop=4 shiftwidth=4

" Configure gui options
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar

"scrolling
set so=5

" text rendering
syntax enable
set background=dark
if has('nvim') " solarized doesn't work well in the terminal, which is where I mostly use nvim
	" TODO: find proper colorscheme for nvim in terminal
else
colorscheme solarized
endif
"colorscheme jellybeans
set guifont=Hack
set encoding=utf-8

" editing
set foldmethod=syntax
set nofoldenable
set backspace=indent,eol,start
set autoread

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

"Managing files
nnoremap <leader>fs :w<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fr :History<CR>
nnoremap <silent> <Leader>fo :<C-U>silent !start explorer %:p:h:S<CR>
nnoremap <silent> <Leader>fc :<C-U>silent !start cmd /c code %:p:h:S & code %<CR>

"Managing buffers
nnoremap <leader>bb :Buffers<CR>
nnoremap <leader>bx :bd<CR>
nnoremap <leader><tab> :b#<CR>

"Managing tabs
nnoremap <leader>ta :tabnew<CR>
nnoremap <leader>tq :tabc<CR>
nnoremap <leader>tQ :tabc!<CR>
nnoremap <leader>t^ :tabfir<CR>
nnoremap <leader>t$ :tablast<CR>
nnoremap <leader>th :tabmove -1<CR>
nnoremap <leader>tl :tabmove +1<CR>
nnoremap <leader>l1 1gt
nnoremap <leader>l2 2gt
nnoremap <leader>l3 3gt
nnoremap <leader>l4 4gt
nnoremap <leader>l5 5gt
nnoremap <leader>l6 6gt
nnoremap <leader>l7 7gt
nnoremap <leader>l8 8gt
nnoremap <leader>l9 9gt

"Managing searches
nnoremap <leader>sc :noh<CR>
if has('nvim') " This plugin doesn't work properly on gvim in windows
nnoremap <leader>sg :FlyGrep<CR>
endif

"Manage version control - Requires fugitive to be set up
" nnoremap <leader>gc :Commits<CR>

" maps the window splitting and resizing
nnoremap <leader>wd <C-w>wq " This needs to be tested
nnoremap <leader>wm <C-w>_<C-w><Bar>
nnoremap <leader>w <C-w>
nnoremap <leader>1 1<C-w>w
nnoremap <leader>2 2<C-w>w
nnoremap <leader>3 3<C-w>w
nnoremap <leader>4 4<C-w>w
nnoremap <leader>5 5<C-w>w
nnoremap <leader>6 6<C-w>w
nnoremap <leader>7 7<C-w>w
nnoremap <leader>7 7<C-w>w
nnoremap <leader>7 7<C-w>w

" Managing toggles
nnoremap <leader>tn :set number!<CR>
nnoremap <leader>tr :set relativenumber!<CR>:set relativenumber?<CR>
nnoremap <leader>au :UndotreeToggle<CR>

" Manage bookmarks
nnoremap <leader>fj <Cmd>call fzf#run(fzf#wrap({
	\ 'source': readfile('../bookmarks'),
	\ 'options': [
		\ '--multi',
	\ ],
\ }))<CR>
nnoremap <leader>fe :e ../bookmarks<CR>

inoremap jj <Esc>
"Misc
" remove trailing whitespace on save
" autocmd BufWritePre *.lua,*.luascn %s/\s\+$

"Consistency changes
nnoremap Y y$

" make n always search forward and N backward
nnoremap <expr> n 'Nn'[v:searchforward]
nnoremap <expr> N 'nN'[v:searchforward]

" make ; always "find" forward and , backward
nnoremap <expr> ; getcharsearch().forward ? ';' : ','
nnoremap <expr> , getcharsearch().forward ? ',' : ';'

"Manage frequent directories
nnoremap <leader>gtd :cd G:\fd\Applications\Mining\VRSM\Data<CR>
nnoremap <leader>gtv :e C:\Users\User\Desktop\gVimPortable\Data\settings\_vimrc<CR>
