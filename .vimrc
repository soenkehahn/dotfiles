
" An example for a vimrc file.
"
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last change:	2011 Apr 15
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"	      for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"	    for OpenVMS:  sys$login:.vimrc

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file
endif
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

" -------------
" My own config
" -------------

" vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'

Plugin 'tomasr/molokai'
Plugin 'flazz/vim-colorschemes'
Plugin 'jkarni/sensei-neovim'
Plugin 'AnsiEsc.vim'
Plugin 'scrooloose/nerdcommenter'

call vundle#end()
filetype plugin indent on

" changing the leader to h
let mapleader = "h"

" pathogen
execute pathogen#infect()

" turning off backup files
set nobackup

" buffer control (with CtrlP)
set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_switch_buffer = 'E'
map <Leader>e :CtrlPBuffer<CR>
map <Leader>o :CtrlPMixed<CR>
" don't split windows when opening another buffer from one with unsaved
" changes
set hidden
" files to be ignored
set wildignore+=*.o,*.hi,dist/build/autogen/cabal_macros.h

" NERDTree
let NERDTreeQuitOnOpen = 1
map <Leader>t :NERDTree<CR>

" tabstop behavior
set smartindent
set tabstop=2
set shiftwidth=2
set expandtab

" insert line without going to insert mode
map <Leader>i O<Esc>j

" highlighting trailing whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" cursorline
" :hi CursorLine cterm=NONE ctermbg=192 ctermfg=NONE
" :set cursorline

set number

" highlighting long lines
:set colorcolumn=81,82
:hi ColorColumn cterm=NONE ctermbg=darkblue ctermfg=NONE

" shortcut for normalize-imports
map <Leader>f :call RestoreCursor('%!normalize-imports') <CR>

inoremap <C-h> <Esc> :call Geany()<CR>
noremap  <C-h>       :call Geany()<CR>
map      <Leader>h   :call Geany()<CR>
map      <Leader>g   :call GeanyNext()<CR>

map <Leader>c :8wincmd ><CR>
map <Leader>l :8wincmd <<CR>

map x :echo "use delete!"<CR>

set undofile

set paste
set nowrap
colorscheme relaxedgreen

set errorformat^=%f:%l:%c:
set errorformat^=%.%.%.%.%.\ \ %f:%l:\ %m

" nerdcommenter
map <Leader>s :call NERDComment(0,"toggle")<CR>
