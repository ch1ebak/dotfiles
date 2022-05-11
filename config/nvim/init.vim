""" PLUGINS

call plug#begin()

" Interface
Plug 'blueyed/vim-diminactive'
Plug 'farmergreg/vim-lastplace'
Plug 'mhinz/vim-startify'
Plug 'junegunn/limelight.vim'
Plug 'ap/vim-css-color'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Color schemes
Plug 'catppuccin/nvim'
Plug 'dracula/vim'
Plug 'arcticicestudio/nord-vim'
Plug 'ackyshake/Spacegray.vim'

" Editing
Plug 'godlygeek/tabular'
Plug 'tomtom/tcomment_vim'
Plug 'Raimondi/delimitMate'

" Searching
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

" Snippets
Plug 'vim-syntastic/syntastic'

call plug#end()



""" GENERAL

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



""" EYE CANDY

colorscheme spacegray 
let g:airline_theme='minimalist'
let g:airline_powerline_fonts = 1


""" CONFIGS

" Limelight

let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
let g:limelight_conceal_guifg = 'DarkGray'
let g:limelight_conceal_guifg = '#777777'
let g:limelight_default_coefficient = 0.7
let g:limelight_paragraph_span = 1
let g:limelight_bop = '^\s'
let g:limelight_eop = '\ze\n^\s'
let g:limelight_priority = -1

" FZF

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction
let g:fzf_action = {
  \ 'ctrl-q': function('s:build_quickfix_list'),
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }
let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6 } }
let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6, 'relative': v:true } }
let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6, 'relative': v:true, 'yoffset': 1.0 } }
let g:fzf_layout = { 'down': '40%' }
let g:fzf_layout = { 'window': 'enew' }
let g:fzf_layout = { 'window': '-tabnew' }
let g:fzf_layout = { 'window': '10new' }
let g:fzf_history_dir = '~/.local/share/fzf-history'

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
