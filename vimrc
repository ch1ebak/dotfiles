"""""""""""""""""""""""""""""""""""
""" https://github.com/ch1ebak/ """
"""""""""""""""""""""""""""""""""""



"""""""""""""""
""" PLUGINS """

call plug#begin()

" color schemes
Plug 'catppuccin/vim', { 'as': 'catppuccin' }
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'sainnhe/everforest', { 'as': 'everforest' }
Plug 'morhetz/gruvbox', { 'as': 'gruvbox' }
Plug 'arcticicestudio/nord-vim', { 'as': 'nord' }
Plug 'joshdick/onedark.vim', { 'as': 'onedark' }
Plug 'altercation/vim-colors-solarized', { 'as': 'solarized' }
Plug 'ghifarit53/tokyonight-vim', { 'as': 'tokyonight' }

" lightline
Plug 'itchyny/lightline.vim'
Plug 'shinchu/lightline-gruvbox.vim'

" idk
Plug 'dense-analysis/ale'
Plug 'tpope/vim-commentary'
Plug 'ap/vim-css-color'
Plug 'Raimondi/delimitMate'
Plug 'ryanoasis/vim-devicons'
Plug 'blueyed/vim-diminactive'
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-eunuch'
Plug 'terryma/vim-expand-region'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'sjl/gundo.vim'
Plug 'instant-markdown/vim-instant-markdown', {'for': 'markdown', 'do': 'yarn install'}
Plug 'farmergreg/vim-lastplace'
Plug 'terryma/vim-multiple-cursors'
Plug 'preservim/nerdtree'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'jceb/vim-orgmode'
Plug 'vim-python/python-syntax'
Plug 'godlygeek/tabular'
Plug 'frazrepo/vim-rainbow'
Plug 'tpope/vim-surround'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'vimwiki/vimwiki'

call plug#end()


""""""""""""""""""""""""""
""" PLUGINS - SETTINGS """

"" Dim inactive
let g:diminactive_use_colorcolumn = 0
let g:diminactive_use_syntax = 1
let g:diminactive_enable_focus = 1

"" Expand region
let g:expand_region_use_select_mode = 1
map K <Plug>(expand_region_expand)
map J <Plug>(expand_region_shrink)

"" Instant markdown
filetype plugin on
let g:instant_markdown_slow = 1
let g:instant_markdown_autostart = 0
let g:instant_markdown_open_to_the_world = 1
let g:instant_markdown_allow_unsafe_content = 1
let g:instant_markdown_allow_external_content = 0
let g:instant_markdown_mathjax = 1
let g:instant_markdown_mermaid = 1
let g:instant_markdown_logfile = '/tmp/instant_markdown.log'
let g:instant_markdown_autoscroll = 0
let g:instant_markdown_port = 8888
let g:instant_markdown_python = 1

"" Last place
let g:lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
let g:lastplace_ignore_buftype = "quickfix,nofile,help"
let g:lastplace_open_folds = 0

"" Multiple cursors
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_start_word_key      = '<C-n>'
let g:multi_cursor_select_all_word_key = '<A-n>'
let g:multi_cursor_start_key           = 'g<C-n>'
let g:multi_cursor_select_all_key      = 'g<A-n>'
let g:multi_cursor_next_key            = '<C-n>'
let g:multi_cursor_prev_key            = '<C-p>'
let g:multi_cursor_skip_key            = '<C-x>'
let g:multi_cursor_quit_key            = '<Esc>'

"" NerdTree
map <C-n> :NERDTreeToggle<CR>
let g:NERDTreeDirArrowExpandable = '►'
let g:NERDTreeDirArrowCollapsible = '▼'
let NERDTreeShowLineNumbers=1
let NERDTreeShowHidden=1
let NERDTreeMinimalUI = 1
let g:NERDTreeWinSize=38

"" Python syntax
let g:python_highlight_all = 1

"" Rainbow
let g:rainbow_active = 1

" Startify
Plug 'mhinz/vim-startify'

" VimWiki
let g:vimwiki_list = [{'path': '~/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]

" Yankstack
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste


""""""""""""""""
""" SETTINGS """ 

set path+=**
set wildmenu
set incsearch
set hidden
set nobackup
set noswapfile
set t_Co=256
set number relativenumber
set clipboard=unnamedplus
syntax enable
let g:rehash256 = 1
syntax on 
set nocompatible
filetype plugin on
set encoding=UTF-8
set mouse=nicr
let g:python_highlight_all = 1
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org 
au BufEnter *.org            call org#SetOrgFileType()

"" GUI
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar

"" Remap keys 
:imap ii <Esc>


"""""""""""""""""""""
""" COLOR SCHEMES """ 

"" available color schemes
" catppuccin_mocha
" dracula
" gruvbox
" nord
" one
" solarized

colorscheme gruvbox 
set background=dark

" Tokyo night
" let g:tokyonight_style = 'night'
" let g:tokyonight_enable_italic = 1

"" Lightline
let g:lightline = {
     \ 'colorscheme': 'gruvbox',
\ }
set laststatus=2
set noshowmode
