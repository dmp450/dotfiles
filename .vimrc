
"Automatic reloading of .vimrc after saving changes
autocmd! bufwritepost .vimrc source %

" Better copy & paste
" When you want to paste large blocks of code into vim, press F2 before
" you paste. At the bottom you should use ``-- INSERT (paste) --``.
"

set pastetoggle=<F2>
set clipboard=unnamed

"Mouse and backspace
set mouse=a
set bs=2

" Set the leader key
let mapleader = ","

" Quicksave command
noremap <Leader>w :update<CR>
vnoremap  <Leader>w :update<CR>
inoremap <Leader>w :update<CR>

" Quick quit command
noremap <Leader>. :quit<CR> "Quit current window
noremap <Leader>> :qa!<CR> "Quit all windows

" Map sort function to a key
vnoremap <Leader>s :sort<CR>

"show numbers
set number
"set tw=80
"set nowrap
"set fo-=t
"highlight ColorColumn ctermbg=magenta
"call matchadd('ColorColumn', '\%81v', 100)
"set colorcolumn=81
"highlight ColorColumn ctermbg=233
set colorcolumn=80

"set tabs to be spaces of 4 by default
set tabstop=4
set expandtab
set shiftwidth=4
"setting sts to -1 gives same behaviour as tabstop. Do this to backspace
"multiple spaces in an expanded tab.
set softtabstop=-1

"set tab sizes to be 8, and tab characters in C, C++, Makefile, Bash
autocmd Filetype c setlocal ts=8 sw=8 noexpandtab
autocmd Filetype cpp setlocal ts=8 sw=8 noexpandtab
autocmd Filetype make setlocal ts=8 sw=8 noexpandtab

"swap the : and ; makes it easier to do : commands since we won't have to
"press shift
noremap ; :
noremap : ;

set autoindent
set cindent

"enable relative line numbers
set relativenumber

"Set the color scheme
colorscheme wombat256mod

call plug#begin()
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
call plug#end()

au BufEnter ~/.mutt/tmp/* :normal }
au BufEnter ~/.mutt/tmp/* :r ~/.mutt/signature
au BufEnter ~/.mutt/tmp/* :normal O
au BufEnter ~/.mutt/tmp/* :startinsert!
