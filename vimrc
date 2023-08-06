""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""
""  _    __ ____ __  ___ ____   ______  ""
"" | |  / //  _//  |/  // __ \ / ____/  ""
"" | | / / / / / /|_/ // /_/ // /       ""
"" | |/ /_/ / / /  / // _, _// /___     ""
"" |___//___//_/  /_//_/ |_| \____/     ""
""                                      ""
""  github.com/ch1ebak/                 ""
""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""


"""""""""""
""VIMPLUG""
"""""""""""
call plug#begin()

Plug 'ap/vim-css-color'
Plug 'csexton/spacemanspiff.vim'
Plug 'dhruvasagar/vim-table-mode'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'farmergreg/vim-lastplace'
Plug 'godlygeek/tabular'
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'matze/vim-move'
Plug 'mbbill/undotree'
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'mhinz/vim-startify'
Plug 'morhetz/gruvbox'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'nordtheme/vim', { 'as': 'nord' }
Plug 'preservim/nerdtree'
Plug 'preservim/vim-markdown'
Plug 'ryanoasis/vim-devicons'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-surround'
Plug 'vim-python/python-syntax'
Plug 'vim-syntastic/syntastic'
Plug 'vimwiki/vimwiki'

call plug#end()


"""""""""""""""""""
""PLUGIN SETTINGS""
"""""""""""""""""""

"fzf
let $FZF_DEFAULT_COMMAND = 'rg --files --hidden'

"last place                                              
let g:lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
let g:lastplace_ignore_buftype = "quickfix,nofile,help"    
let g:lastplace_open_folds = 0                             

"lightline
let g:lightline = {
      \ 'colorscheme': 'apprentice',
      \ }
let g:lightline.active = {
    \ 'left': [ [ 'mode', 'paste' ],
    \           [ 'readonly', 'filename', 'modified' ] ],
    \ 'right': [ [ 'lineinfo' ],
    \            [ 'percent' ] ] }
let g:lightline.inactive = {
    \ 'left': [ [ 'filename' ] ],
    \ 'right': [ [ 'lineinfo' ],
    \            [ 'percent' ] ] }
let g:lightline.tabline = {
    \ 'left': [ [ 'tabs' ] ],
    \ 'right': [ [ 'close' ] ] }

"markdown                                                    
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh']
let g:markdown_syntax_conceal = 0                              
let g:markdown_minlines = 100                                  

"nerdtree
let g:NERDTreeDirArrowExpandable = '►' 
let g:NERDTreeDirArrowCollapsible = '▼'
let NERDTreeShowLineNumbers=1          
let NERDTreeShowHidden=1               
let NERDTreeMinimalUI = 1              
let g:NERDTreeWinSize=38               
let NERDTreeBookmarksFile=expand("$HOME/.vim/custom/NERDTreeBookmarks")

"python syntax              
let g:python_highlight_all = 1

"startify
let g:startify_custom_header =
                     \ startify#pad(readfile('/home/kd/.vim/custom/vim-ascii.txt'))
let g:startify_session_ignore_files = 1
let g:startify_bookmarks = systemlist("cut -sd' ' -f 2- ~/.vim/custom/NERDTreeBookmarks")
let g:startify_lists = [
      \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
      \ ]

"syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"table mode
let g:table_mode_corner='|'

"undotree
if has("persistent_undo")
    set undodir=~/.vim/custom/undodir
    set undofile
endif

"vimwiki
let g:vimwiki_list = [{'path': '~/Dokumenty/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]


""""""""""""
""SETTINGS""
""""""""""""
set nocompatible         

"misc
set autoread
set confirm
set hidden
set wildmenu             

"code
set nofoldenable

"GUI
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar

"indentation
set autoindent
set expandtab
set tabstop=2

"search
set hlsearch
set smartcase
set ignorecase
set incsearch

"text
set encoding=UTF-8
set linebreak
syntax enable
set wrap

"theme
colorscheme gruvbox 
" let g:dracula_colorterm = 0
set background=dark
set t_Co=256             
let g:rehash256 = 1      
set notermguicolors

"ui
set laststatus=2
set noshowmode
set ruler
set number relativenumber
set mouse=a


"""""""""""""""
""KEYBINDINGS"" 
"""""""""""""""
let g:mapleader = "\<Space>"
nmap <leader>hr :source .vimrc<Cr>

"aspell
nmap <leader>ap :!aspell --lang=pl -c %<Cr>
nmap <leader>ae :!aspell --lang=en -c %<Cr>

"fzf
nmap <leader>sf :Files<Cr>
nmap <leader>sr :Rg<Cr>
nmap <leader>sb :BLines<Cr>

"nerdtree
nnoremap <leader>. :NERDTreeToggle<CR>  
nnoremap <leader>.. :NERDTreeFocus<CR>  

"splits
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nmap <leader>v :vsplit
nmap <leader>s :split

"undotree
nnoremap <leader>u :UndotreeToggle<CR>
nnoremap <leader>uu :UndotreeFocus<CR>
