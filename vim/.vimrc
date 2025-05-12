runtime defaults.vim
set directory=/tmp
set viminfo+=n~/.cache/vim/viminfo

set tabstop     =4
set softtabstop =4
set shiftwidth  =4
set expandtab

autocmd TextYankPost * if (v:event.operator == 'y' || v:event.operator == 'd') | silent! execute 'call system("wl-copy", @")' | endif
nnoremap p :let @"=substitute(system("wl-paste --no-newline"), '<C-v><C-m>', '', 'g')<cr>p
