"Tab related
set tabstop=2
set shiftwidth=2
set expandtab

map ~ctmpl i#include<stdio.h><CR><CR>int main(int argc,char *argv[],char *envp[])<CR>{<CR><CR>}<CR><Esc>kka<Tab>
map ~cpptmpl i#include<iostream><CR><CR>using namespace std;<CR><CR>int main(int argc,char *argv[],char *envp[])<CR>{<CR><CR>return 0;<CR>}<CR><Esc>kka<Tab

"Pathogen
execute pathogen#infect()

"Search related
set incsearch
set hlsearch
set smartcase


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

"color scheme, eyeCandy
syntax enable 
"highlight Comment ctermfg=cyan cterm=none

if has('gui_running')
  set background=light
  colorscheme solarized 
  set guioptions-=m  "remove menu bar
  set guioptions-=T  "remove toolbar
  set guioptions-=r  "remove right scroll bar
endif

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

nnoremap <Leader>u :GundoToggle<CR>

set relativenumber
set number

"windo set scrollbind
"windo set noscrollbind
"nmap <F5> :windo set scrollbind!<cr>

nnoremap \fs :Ag! <C-R>=expand("<cword>")<CR><CR>	
nnoremap \ff :AgFile! <C-R>=expand("<cfile>")<CR><CR>	

filetype plugin on 
