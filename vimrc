"vim-plug installation
"curl -fLo ~/.vim/autoload/plug.vim --create-dirs\
"    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
"curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
"    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/plugged')
 Plug 'tpope/vim-fugitive'
 Plug 'junegunn/gv.vim'

 Plug 'tpope/vim-repeat'
 Plug 'tpope/vim-rsi'
 Plug 'tpope/vim-unimpaired'
 Plug 'nelstrom/vim-visual-star-search'

 Plug 'tpope/vim-surround'
 Plug 'tpope/vim-commentary'
 Plug 'tommcdo/vim-exchange'
 Plug 'tommcdo/vim-lion'
 Plug 'wellle/targets.vim'

 Plug 'sjl/gundo.vim'
 Plug 'mbbill/undotree'
 Plug 'rbgrouleff/bclose.vim'
 Plug 'tyru/open-browser.vim'
 Plug 'AndrewRadev/undoquit.vim'
 Plug 'yssl/QFEnter'
 Plug 'majutsushi/tagbar'
 Plug 'osyo-manga/vim-anzu'
 Plug 'romainl/vim-cool'
 Plug 'junegunn/vim-peekaboo'
 Plug 'tyru/capture.vim'
 Plug 'chrisbra/NrrwRgn'
 Plug 'tpope/vim-characterize'
 Plug 'tpope/vim-speeddating'

 Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
 Plug 'junegunn/fzf.vim'
 Plug 'srstevenson/vim-picker'
 Plug 'mhinz/vim-grepper'

 "Plug 'altercation/vim-colors-solarized'
 Plug 'lifepillar/vim-solarized8'
 Plug 'jnurmine/Zenburn'
 Plug 'chriskempson/vim-tomorrow-theme'

 Plug 'vim-scripts/scratch.vim'
 Plug 'terryma/vim-expand-region'
 Plug 'ConradIrwin/vim-bracketed-paste'
 Plug 'unblevable/quick-scope'
 " Plug 'romainl/vim-qf'
 "------------------------------------------------------------------------------
call plug#end()

"let g:rsi_no_meta = 1
let g:solarized_diffmode="high"
"let g:solarized_termcolors=256

""vim-grepper
let g:grepper = { 'tools': ['rg', 'ag', 'ack', 'grep'] }

""quick-scope
let g:qs_highlight_on_keys = ['f', 'F']

""open-browser.vim
let g:netrw_nogx = 1 " disable netrw's gx mapping.
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)

""vim-anzu
nmap n <Plug>(anzu-n-with-echo)
nmap N <Plug>(anzu-N-with-echo)
nmap * <Plug>(anzu-star-with-echo)
nmap # <Plug>(anzu-sharp-with-echo)

function! FallBackToGrepperIfNoCscope(word, openVertical)
  if cscope_connection()
    if a:openVertical
      execute 'vertical scs f t' a:word
    else
      execute 'cs f t' a:word
    endif
  else
    execute 'Grepper -noprompt -cword'
  endif
endfunction

function! QuickfixToggle() "{{{
    let nr = winnr("$")
    cwindow
    let nr2 = winnr("$")
    if nr == nr2
        cclose
    endif
endfunction "}}}

"Tab related
set tabstop=2
set shiftwidth=2
set expandtab

set nrformats=hex

"Search related
set incsearch
set hlsearch
set ignorecase
set smartcase

set backspace=indent,eol,start

"Indent related
set autoindent
set smartindent
set cindent
set smarttab

set showcmd
set ruler     " shows line no, posn, etc
set showmatch "highlights matching brace for a brief moment

"Keeps the cursor in the middle while scrolling allows to see the contents above and below the cursor
set scrolloff=10
set laststatus=2

"color scheme, eyeCandy
syntax enable

if has("termguicolors")
  let &t_8f="\e[38;2;%lu;%lu;%lum"
  let &t_8b="\e[48;2;%lu;%lu;%lum"
  set termguicolors
endif

"highlight Comment ctermfg=cyan cterm=none

if has('gui_running')
  set background=light
  colorscheme solarized8_light
  set guioptions-=m  "remove menu bar
  set guioptions-=T  "remove toolbar
  set guioptions-=r  "remove right scroll bar
else
  set background=dark
  colorscheme Tomorrow-Night
endif

"-------------------------------------------------------------------------------
" Custom Key Maps

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

vnoremap < <gv
vnoremap > >gv

nnoremap Y y$
nmap gs  <plug>(GrepperOperator)
xmap gs  <plug>(GrepperOperator)

" run a macro
nnoremap Q @q
" run a macro on visually selected lines
xnoremap Q :normal @q <CR>

"inoremap <c-w> <c-g>u<c-w>
inoremap <c-u> <c-g>u<c-u>
inoremap <c-k> <c-o>d$
inoremap <c-_> <c-o>u

" nnoremap <SPACE> <Nop>
" let mapleader = "\<Space>"
map <SPACE> <Leader>

nnoremap <Leader>u :GundoToggle<CR>
nnoremap <Leader>U :UndotreeToggle<CR>

nnoremap <Leader>b :PickerBuffer<CR>
nnoremap <Leader>p :PickerEdit<CR>
nnoremap <Leader>x :Commands<CR>
nnoremap <Leader>m :Marks<CR>
nnoremap <Leader>r :source ~/.vimrc<CR>
nnoremap <Leader>t :TagbarToggle<CR>
nnoremap <Leader>w :q<CR>
nnoremap <Leader>q :Bclose<CR>
nnoremap <Leader>z :call QuickfixToggle()<CR>
nnoremap <Leader>/ :Grepper -buffer -noprompt -query '<C-r>/'<CR>

nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gvdiff<CR>
nnoremap <Leader>gl :GV<CR>
nnoremap <Leader>gf :GV!<CR>
nnoremap <Leader>gh :Glog<CR>

nnoremap <Leader>du :diffupdate<CR>

nnoremap <Leader>fs :Grepper<CR>
nnoremap <Leader>fv :execute 'e ' . resolve(expand($MYVIMRC))<CR>

if has("cscope")
  " use both cscope and ctag for 'ctrl-]', ':ta', and 'vim -t'
  set cscopetag

  " check cscope for definition of a symbol before checking ctags: set to 1
  " if you want the reverse search order.
  set csto=0

  " add any cscope database in current directory
  if filereadable("cscope.out")
    cs add cscope.out
  " else add the database pointed to by environment variable
  elseif $CSCOPE_DB != ""
    cs add $CSCOPE_DB
  endif

endif

"   's'   symbol: find all references to the token under cursor
"   'g'   global: find global definition(s) of the token under cursor
"   'c'   calls:  find all calls to the function name under cursor
"   't'   text:   find all instances of the text under cursor
"   'e'   egrep:  egrep search for the word under cursor
"   'f'   file:   open the filename under cursor
"   'i'   includes: find files that include the filename under cursor
"   'd'   called: find functions that function under cursor calls
nnoremap <Leader>ca :cs add cscope.out<CR>
nnoremap <Leader>ct :call FallBackToGrepperIfNoCscope(expand("<cword>"), 0)<CR>
nnoremap <Leader>cs :cs find s <C-R>=expand("<cword>")<CR>
nnoremap <Leader>cg :cs find g <C-R>=expand("<cword>")<CR><CR>
nnoremap <Leader>vt :call FallBackToGrepperIfNoCscope(expand("<cword>"), 1)<CR>
nnoremap <Leader>vs :vertical scs find s <C-R>=expand("<cword>")<CR>
nnoremap <Leader>vg :vertical scs find g <C-R>=expand("<cword>")<CR>
set cscopequickfix=s-,c-,d-,i-,t-,e-
"http://vim.wikia.com/wiki/Automatically_open_the_quickfix_window_on_:make
augroup autoOpenQuickFix
  autocmd!
  autocmd QuickFixCmdPost [^l]* nested cwindow
  autocmd QuickFixCmdPost    l* nested lwindow
augroup END

"-------------------------------------------------------------------------------
" Recorded Macros

"To bring up a vimdiff with next modified file in fugitive
let @c = ' gsdv'

set number
set relativenumber

"windo set scrollbind
"windo set noscrollbind
"nmap <F5> :windo set scrollbind!<cr>

if has('nvim')
  "disable mouse
  set mouse=
  set inccommand=split
else
  "Slow start time in terminal - X11 clipboard
  set clipboard=exclude:.*
endif

filetype plugin on

