
syntax on
filetype plugin indent on
" Settings

set showcmd			" Show (partial) command in status line.
set showmatch		" Show matching brackets.
set ignorecase		" Do case insensitive matching
set incsearch		" Incremental search
set number			" Relative numbers
set tabstop=4		" Set tabs to 4 instead of 8

" Mapping

nmap <silent> <A-Up> :wincmd k<CR>
nmap <silent> <A-Down> :wincmd j<CR>
nmap <silent> <A-Left> :wincmd h<CR>
nmap <silent> <A-Right> :wincmd l<CR>

set mouse=a
map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>
