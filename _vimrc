" Enable python 3 support in theory
" :let &pythonthreedll = '..\vimfiles\python36.dll'

" This section manages plug in packages using plug.vim
" https://github.com/junegunn/vim-plug
" NOTE : The plug.vim file must be added to your autoload directory before
" this configuration will work. More detailed installation instructions are available
" on the github link.
call plug#begin()

" Note : The plug.vim syntax points to the repository for the plug in on
" github. The string is formatted as '<NameOfUser>/<NameOfRepo>'. The repos
" will have more info on their respective plugins


" This adds more movement and editing commands for convenience. See Repo for
" details.
Plug 'tpope/vim-unimpaired'

" This adds bindings to comment and uncomment code in a variety of languages.
Plug 'tpope/vim-commentary'

" This provides functionality to work with parenthesis, brackets, quotes and
" generally matched pairs that surround other text. I recommend visiting the
" repo for examples - it will be much more clear.
Plug 'tpope/vim-surround'

" This allows the vim 'repeat previous action/edit' command to work with the
" other tpope plugins. 
Plug 'tpope/vim-repeat'

" This adds convenience bindings to use the vim directory manager / file
" explorer.
Plug 'tpope/vim-vinegar'

" Provides bindings to use emmet, a popular autocomplete / templating syntax
" for html / xml files. Google can provide many tutorials.
Plug 'mattn/emmet-vim'

" This provides a quick way to search for a combination of characters near the
" cursor.
Plug 'justinmk/vim-sneak'

" This provides a syntax for note taking in plain text files. It also provides
" functionality to export those note to html, pdf, etc.
Plug 'vimwiki/vimwiki'

" This provides a visual way to see your undo changes. Every time you undo an
" edit, then make a new edit the history of the file has branched (similar to
" a version control system).
Plug 'mbbill/undotree'

" This adds support for syntax highlighting and basic linting for a multitude
" of language. The repository has up to date info on the supported languages.
Plug 'sheerun/vim-polyglot'

" This changes the word-wise movement to treat camel case words as multiple
" separate words instead of one single word.
Plug 'chaoren/vim-wordmotion'

" This is a colorscheme, the defualt one that is loaded with this file.
Plug 'altercation/vim-colors-solarized'

" This is a darker backup theme I sometimes use. You can switch to it with the
" command ':colorscheme jellybeans'
Plug 'nanotech/jellybeans.vim'

" This sets a bar at the bottom of the screen that displays useful information
" such as location in a faile, linting errors, etc. The information can be
" customized.
Plug 'vim-airline/vim-airline'

" This provides colorschemes for the helpul indormation bar at the bottom (see
" previous comment).
Plug 'vim-airline/vim-airline-themes'

" This plugin is similar to Zen Mode in VSCode. It will add padding to the
" file and center it to ease focuse. Can be toggled with the command ':Goyo'
Plug 'junegunn/goyo.vim'

" This plugin dims paragraphs that do not have the cursor in them to ease
" focus on the current paragraph. Useful for code reviews. Is currently
" configured to start whn goyo does, but can be turned on manually with the
" command ':Limelight', and turned off with ':Limelight!'
Plug 'junegunn/limelight.vim'

" This integrates with the linux (or wsl) fzf utility. It allows you to find files in
" the current directory with a fuzzy search. This plugin may be the most
" difficult to install depending on your system, see the repository for any issues
" and for examples.
Plug 'junegunn/fzf', { 'dir': '../fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" This is a search utility for finding content in files. It does not seem to
" work well on windows
Plug 'wsdjeg/FlyGrep.vim'

" This plug in allows you to interact with various version control systems
" from inside vim. Use the command ':help VCS' to open a split with
" instructions on the commands
Plug 'vim-scripts/vcscommand.vim'

" NOTE : This plug in has a special syntax because there is a different version based
" on if you are using vim or neovim.
" This plug in highlights changes that have been made in version controlled
" files. It also allows you to easily jump between those changes, and provides
" other useful utilities
if has('nvim') || has('patch-8.0.902') "Requires async support
  Plug 'mhinz/vim-signify'
else
  Plug 'mhinz/vim-signify', { 'branch': 'legacy' }
endif

" These are a autocomplete plug-ins that allows you to add snippet files. I
" am still experimenting and trying to make part of my configuration. For
" anyone copying my _vimrc, feel free to remove this section as it is only for
" my use.
"===============================================================================
"Plug 'lifepillar/vim-mucomplete'
"Plug 'ycm-core/YouCompleteMe'

"This is required for snippets with snipmate
"Plug 'MarcWeber/vim-addon-mw-utils'
"Plug 'tomtom/tlib_vim'
"Plug 'garbas/vim-snipmate'

" Track the engine.
"Plug 'SirVer/ultisnips'

" This contains snippets
" Plug 'honza/vim-snippets'
"===============================================================================

call plug#end()

"===============================================================================
" Configuration for my plugins starts here
"===============================================================================

"== This is the Vimwiki configuration section ==
" These are the configurations for the vim-wiki plug in. They are essentially
" the default provided with the plugin.
 " let wiki_settings={
 " \ 'template_path': vimwiki_export_path',
 " \ 'template_default': 'default',
 " \ 'template_ext': '.html',
 " \ 'auto_export': 0,
 " \ 'nested_syntaxes': {
 " \ 'js':'javascript',
 " \ }}

"== This is the AutoComplete configuration section ==
" These are a autocomplete plug-ins that allows you to add snippet files. I
" am still experimenting and trying to make part of my configuration. For
" anyone copying my _vimrc, feel free to remove this section as it is only for
" my use.
"===============================================================================
"Auto complete configuration for mucomplete
" let g:mucomplete#enable_auto_at_startup = 1
" let g:mucomplete#minimum_prefix_length = 3
" let g:mucomplete#buffer_relative_paths = 1
" set completeopt=menuone,noselect,noinsert shortmess+=c
" inoremap <expr> <CR> pumvisible() ? "\<C-Y>\<CR>" : "\<CR>"

" Trigger configuration for ultisnip snippets. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
"let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsJumpForwardTrigger="<c-b>"
"let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Mappings for snipmate
":imap <Tab> <Plug>snipMateNextOrTrigger
"===============================================================================

"== This is the Goyo and Limelight configuration section ==
" These commands set Limelight to be enabled and disabled when Goyo is started,
" or exited.
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!


"== This is the Airline configuration section ==
" Sets the theme for airline
let g:airline_theme='jellybeans'


"== This is the Emmet configuration section ==
" This changes the key that performs an emmet - style autocomplete.
"let g:user_emmet_mode='a'
"let g:user_emmet_leader_key='<Leader>'

"== This is the Signify configuration section ==
" These are some behaviour settings for signify
let g:signify_disable_by_default = 0
let g:signify_line_highlight = 0
" These are new keybindings to call the Signify functions
nnoremap <Leader>fvt :SignifyToggle<CR>
nnoremap <Leader>fvd :SignifyDiff<CR>
nnoremap <Leader>fvh :SignifyToggleHighlight<CR>

"===============================================================================
" Configuration for my plugins ends here
"===============================================================================


"===============================================================================
" Configuration for built-in Vim functionality starts here
"===============================================================================

" gVim plays an error sound sometimes when typing. This is a workaround to
" stop the beeping. It disables sound from constant errors while typing 
" I would like to solve the errors, but I'm not sure how currently
autocmd GUIEnter * set vb t_vb=

" Manage language support for custom file extensions
" These are not common file extensions for lua and can be removed. If you wish
" to work with lua I recommend keeping only the first line that sets nospell.
autocmd BufNewFile,BufRead *.lua set nospell
autocmd BufNewFile,BufRead *.luascn set syntax=lua
autocmd BufNewFile,BufRead *.luascn set nospell
autocmd BufNewFile,BufRead *.luascn setlocal commentstring=--\ %s
autocmd BufNewFile,BufRead *.smeta set syntax=lua
autocmd BufNewFile,BufRead *.smeta set nospell
autocmd BufNewFile,BufRead *.smeta setlocal commentstring=--\ %s

" The following section deals with built in vim settings, for a more detailed
" explanation of them see the following link
" basic settings - https://www.shortcutfoo.com/blog/top-50-vim-configuration-options/

"== Search Settings ==
" These settings configure how searching behaves in vim
set ignorecase
set smartcase
set incsearch
set hlsearch

"== Indentation Settings ==
" These settings configure indentation of tabs and spaces. Configure according
" to the standards your codebase prefers
set autoindent
set tabstop=4 shiftwidth=0
"set softabstop=4 shiftwidth=4

" ==gVim GUI Settings ==
" These settings configure the GUI interface for gVim
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar

"== Scrolling Settings ==
set so=5

"== Text Visuals and Interface Settings ==
syntax enable
set background=dark
if has('nvim') " solarized doesn't work well in the terminal, which is where I mostly use nvim
	"colorscheme jellybeans
	" TODO: find proper colorscheme for nvim in terminal
else
	colorscheme solarized
endif
set guifont=Hack
set encoding=utf-8
set foldmethod=syntax
set nofoldenable
set backspace=indent,eol,start
set autoread
set number
set relativenumber

" store swap files
" set directory='../swap' this does not work but i want it to

" Hide files that are not open instead of closing them
set hidden

" Spell checking
set spell

"== Shell Settings ==
" Set the shell used
"if has("win32")
"    set shell=C:\Windows\Sysnative\wsl.exe
"    set shellpipe=|
"    set shellredir=>
"    set shellcmdflag=
"endif

"== Key Binding and Macro Settings ==
" NOTE : These do not contains all of the keybindings in this _vimrc file. Plug
" in specific bindings occur under their plug in sections.

" The leader key is a way of creating shortcut commands as a sequence of
" pressed keys, instead of multiple keys held simultaneously. I have set my
" leader key to spacebar because it is easy to reach with both hands, and the
" thumbs don't have much else do to anyway.
let mapleader = " "

"== Insert Mode Mappings ==
" These mappings will only work in insert mode
inoremap <C-x> <Esc>

inoremap <C-h> <C-o><Left>
inoremap <C-j> <C-o><Down>
inoremap <C-k> <C-o><Up>
inoremap <C-l> <C-o><Right>

"== Normal Mode Mappings ==
" These mappings will only work in Normal mode

" Managing buffers and sessions, starts with a q as in 'quit' mnemonic
nnoremap <Leader>qq :q!<CR>
nnoremap <Leader>qQ :qa!<CR>

" Managing files, starts with f as in 'file' mnemonic
nnoremap <Leader>fs :w<CR>
nnoremap <Leader>ff :Files<CR>
nnoremap <Leader>fr :History<CR>
nnoremap <silent> <Leader>fo :<C-U>silent !start explorer %:p:h:S<CR>
nnoremap <silent> <Leader>fc :<C-U>silent !start cmd /c code %:p:h:S & code %<CR>

" Managing buffers, starts with a b as in 'buffer' mnemonic
nnoremap <Leader>bb :Buffers<CR>
nnoremap <Leader>bx :bd<CR>
nnoremap <Leader><tab> :b#<CR>

" Managing tabs, starts with a t as in 'tab' mnemonic
nnoremap <Leader>ta :tabnew<CR>
nnoremap <Leader>tq :tabc<CR>
nnoremap <Leader>tQ :tabc!<CR>
nnoremap <Leader>t^ :tabfir<CR>
nnoremap <Leader>t$ :tablast<CR>
nnoremap <Leader>th :tabmove -1<CR>
nnoremap <Leader>tl :tabmove +1<CR>
" Starts with an l as in 'layout' mnemonic
nnoremap <Leader>l1 1gt
nnoremap <Leader>l2 2gt
nnoremap <Leader>l3 3gt
nnoremap <Leader>l4 4gt
nnoremap <Leader>l5 5gt
nnoremap <Leader>l6 6gt
nnoremap <Leader>l7 7gt
nnoremap <Leader>l8 8gt
nnoremap <Leader>l9 9gt

" Managing searches, starts with an s as in 'search' mnemonic
nnoremap <Leader>sc :noh<CR>
if has('nvim') " This plugin doesn't work properly on gVim in windows
	nnoremap <Leader>sg :FlyGrep<CR>
endif
" This is a custom mapping that will search the file for the last copied text
nnoremap <Leader>sf /<C-r>0<CR>

" Maps the window splitting and resizing, starts with w as in 'window' mnemonic
nnoremap <Leader>w <C-w>
nnoremap <Leader>wm <C-w>_<C-w><Bar>
" Numbers are used without the window mnemonic for convenience
nnoremap <Leader>1 1<C-w>w
nnoremap <Leader>2 2<C-w>w
nnoremap <Leader>3 3<C-w>w
nnoremap <Leader>4 4<C-w>w
nnoremap <Leader>5 5<C-w>w
nnoremap <Leader>6 6<C-w>w
nnoremap <Leader>7 7<C-w>w
nnoremap <Leader>7 7<C-w>w
nnoremap <Leader>7 7<C-w>w

" Managing toggles
nnoremap <Leader>tn :set number!<CR>
nnoremap <Leader>tr :set relativenumber!<CR>:set relativenumber?<CR>
nnoremap <Leader>au :UndotreeToggle<CR>

"===============================================================================
" Configuration for built-in Vim functionality ends here
"===============================================================================


"===============================================================================
" Configuration for custom Vim functions and new features starts here
"===============================================================================

"== This is the Custom Bookmark section ==
" This is an attempt to bookmark files by a custom entered name, currently it
" is incomplete
" nnoremap <Leader>fj <Cmd>call fzf#run(fzf#wrap({
" 	\ 'source': readfile('../bookmarks'),
" 	\ 'options': [
" 		\ '--multi',
" 	\ ],
" \ }))<CR>
" nnoremap <Leader>fe :e ../bookmarks<CR>

"== This is the Format on Save section ==
" remove trailing whitespace on save
" autocmd BufWritePre *.lua,*.luascn %s/\s\+$

"== This is the Consistency Changes section ==
" Here I try to change built in vim behaviour in a way that makes some aspects
" more predictable or consistent

" This lets Y copy the rest of the line the same way C and D operate on the
" rest of the line
nnoremap Y y$

" Make n always search forward and N backward
nnoremap <expr> n 'Nn'[v:searchforward]
nnoremap <expr> N 'nN'[v:searchforward]

" Make ; always 'find' forward and , backward
nnoremap <expr> ; getcharsearch().forward ? ';' : ','
nnoremap <expr> , getcharsearch().forward ? ',' : ';'

"===============================================================================
" Configuration for custom Vim functions and new features ends here
"===============================================================================

"== This is the Convenient Navigations section ==
" This is just for me to easily get to directories I use often. Feel free to
" customize as needed. I do recommend the gtv and gtr binding for everyone 
" who enjoys adding and changing functionality
" Uses the gt as in 'go to' mnemonic

nnoremap <Leader>gtd :cd G:\fd\Applications\Mining\VRSM\Data<CR>
nnoremap <Leader>gtv :e C:\Users\User\Desktop\gVimPortable\Data\settings\_vimrc<CR>
nnoremap <Leader>gtr :so C:\Users\User\Desktop\gVimPortable\Data\settings\_vimrc<CR>
