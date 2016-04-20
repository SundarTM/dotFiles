call plug#begin('~/.vim/plugged')
 Plug 'git://github.com/tpope/vim-fugitive'
 Plug 'git://github.com/tpope/vim-repeat'
 Plug 'git://github.com/tpope/vim-surround'
 Plug 'git://github.com/tpope/vim-rsi' 
 Plug 'git://github.com/tpope/vim-commentary'
 Plug 'git://github.com/tpope/vim-unimpaired'
 Plug 'git://github.com/tpope/vim-dispatch'
 Plug 'git://github.com/sjl/gundo.vim'
 Plug 'git://github.com/junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
 Plug 'git://github.com/junegunn/fzf.vim'
 Plug 'git://github.com/mileszs/ack.vim'
 Plug 'git://github.com/rking/ag.vim'
 Plug 'git://github.com/ConradIrwin/vim-bracketed-paste'

 Plug 'git://github.com/altercation/vim-colors-solarized'
 
 Plug 'git://github.com/tyru/open-browser.vim'
 Plug 'git://github.com/tyru/capture.vim'
 Plug 'git://github.com/vim-scripts/scratch.vim'
 Plug 'git://github.com/chrisbra/NrrwRgn'
 Plug 'git://github.com/mbbill/undotree'
 Plug 'git://github.com/wellle/targets.vim'
 Plug 'git://github.com/rbgrouleff/bclose.vim'
 Plug 'git://github.com/AndrewRadev/undoquit.vim'

 "------------------------------------------------------------------------------
call plug#end()

"let g:rsi_no_meta = 1
let g:solarized_diffmode="high"
"let g:solarized_termcolors=256

""open-browser.vim
let g:netrw_nogx = 1 " disable netrw's gx mapping.
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)

function! DeleteHiddenBuffers()
  "Tabs not cosidered. walks through entire Range
  let tpbl=[]
  let closed = 0
  for buf in range(1, bufnr('$'))
    if ( buf != bufnr("__Scratch__") ) && buflisted(buf) && (bufwinnr(buf) < 0)
      "hidden buffer
      silent execute 'bdelete' buf
      let closed += 1
    endif
  endfor
  echo "Closed ".closed." hidden buffers"
endfunction

function! FallBackToAckIfNoCscope(word, openVertical)
  if cscope_connection()
    if a:openVertical
      execute 'vertical scs f t' a:word
    else
      execute 'cs f t' a:word
    endif
  elseif executable('ag')
    "a:openVertical == 1? execute 'Ag!' a:word : execute 'Ag' a:word
    if a:openVertical
      execute 'Ag!' a:word
    else
      execute 'Ag' a:word
    endif
  else
    if a:openVertical
      execute 'Ack!' a:word
    else
      execute 'Ack' a:word
    endif
  endif
endfunction

"Tab related
set tabstop=2
set shiftwidth=2
set expandtab

"Search related
set incsearch
set hlsearch
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
"highlight Comment ctermfg=cyan cterm=none

if has('gui_running')
  set background=light
  colorscheme solarized 
  set guioptions-=m  "remove menu bar
  set guioptions-=T  "remove toolbar
  set guioptions-=r  "remove right scroll bar
else
  set background=dark
  colorscheme solarized 
endif

"-------------------------------------------------------------------------------
" Custom Key Maps

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

vnoremap < <gv
vnoremap > >gv

" nnoremap <SPACE> <Nop>
" let mapleader = "\<Space>"
map <SPACE> <Leader>

nnoremap <Leader>u :GundoToggle<CR>
nnoremap <Leader>U :UndotreeToggle<CR>

nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>e :Files 
nnoremap <Leader>p :GitFiles<CR>
nnoremap <Leader>x :Commands<CR>
nnoremap <Leader>m :Marks<CR>
nnoremap <Leader>r :source ~/.vimrc<CR>
nnoremap <Leader>w :q<CR>
nnoremap <Leader>q :Bclose<CR>
nnoremap <Leader>y :reg<CR>

nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gvdiff<CR>
nnoremap <Leader>gl :Glog<CR>

nnoremap <Leader>du :diffupdate<CR>
nnoremap <Leader>dh :call DeleteHiddenBuffers()<CR>

nnoremap <Leader>fs :Ag! <C-R>=expand("<cword>")<CR><CR>	
nnoremap <Leader>ff :Files<CR>
nnoremap <Leader>fh :Files ~<CR>
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
nnoremap <Leader>ct :call FallBackToAckIfNoCscope(expand("<cword>"), 0)<CR>
nnoremap <Leader>cs :cs find s <C-R>=expand("<cword>")<CR>
nnoremap <Leader>cg :cs find g <C-R>=expand("<cword>")<CR><CR>
nnoremap <Leader>vt :call FallBackToAckIfNoCscope(expand("<cword>"), 1)<CR>
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

"Slow start time in terminal - X11 clipboard
set clipboard=exclude:.*

filetype plugin on 
