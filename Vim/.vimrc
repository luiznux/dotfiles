let base16colorspace=256

" Enable the syntax
syntax on
filetype plugin indent on

set laststatus=2

set noshowmode

let g:jsx_ext_required = 0

" Activates simplylFold
let g:SimpylFold_docstring_preview=1

" set autoclosing tags in filenames like *.html, *.xhtml
let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.php"

" Set the files encoding to UTF_8
set encoding=utf-8

set expandtab
set tabstop=4
" Indents will have a width of 4
set shiftwidth=4
" Sets the number of columns for a TAB
set softtabstop=4
" Sets the auto indent to true
set autoindent
" Smart indent
set si

retab

" Enables visual wrapping
set wrap

" Turns off Physical line wrapping
set textwidth=0 wrapmargin=0

" highlight matching [{()}]
set showmatch

" Sets the file format to be Unix
set fileformat=unix

" Highlights the current line
set cursorline

" Shows numbe line
set number

" Always show current position
set ruler

set showcmd

" set which area of the screen is gonna split
set splitbelow
set splitright

" Enable folding
set foldmethod=indent
set foldlevel=99

"set show mode(INSERT, VISUAL)

set showmode

"set research highlights

set hlsearch  

" Enable folding with the spacebar
nnoremap <space> za

" Specific config for python files
let python_highlight_all = 1

" Adds PEP8 proper indentation.
au BufNewFile,BufRead *.py:
        \ set shiftwidth=4
        \ set textwidth=79
        \ set autoindent
        \ set fileformat=unix
        \ set tabstop=4
        \ set expandtab
		\ retab

" Adds the correct indentation to javascript, html and css files.
au BufNewFile,BufRead *.html:
        \ set tabstop=4
        \ set softtabstop=4
        \ set shiftwidth=4
        \ set fileformat=unix

autocmd Filetype javascript.jsx setlocal ts=4 sts=4 sw=4

au BufNewFile,BufRead *.css:
        \ set tabstop=2
        \ set softtabstop=2
        \ set shiftwidth=2
        \ set fileformat=unix

" Disables the arrow keys
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>


set termguicolors


" Sets the colorscheme to onedark
set t_Co=256
set background=dark
highlight Normal ctermbg=NONE
highlight nonText ctermbg=NONE


execute pathogen#infect()
call pathogen#helptags()

set noshowmode

colorscheme base16-tomorrow-night


" using Source Code Pro
set anti enc=utf-8
set guifont=Source\ Code\ Pro\ 11

autocmd vimenter * NERDTree
