runtime defaults.vim
set viminfo+=n~/.cache/vim/viminfo

set tabstop     =4
set softtabstop =4
set shiftwidth  =4
set expandtab

" xnoremap <silent> <leader>y y:call system("wl-copy --trim-newline", @")<cr>:call system("wl-copy --trim-newline", @*)<cr>
" vmap <silent> y y:call system("wl-copy", @@)<CR>
" xnoremap "+y y:call system("wl-copy", @"

" xnoremap "+y y:call system("wl-copy", @")<cr>
" nnoremap "+p :let @"=substitute(system("wl-paste --no-newline"), '<C-v><C-m>', '', 'g')<cr>p
" nnoremap "*p :let @"=substitute(system("wl-paste --no-newline --primary"), '<C-v><C-m>', '', 'g')<cr>p


autocmd TextYankPost * if (v:event.operator == 'y' || v:event.operator == 'd') | silent! execute 'call system("wl-copy", @")' | endif
nnoremap p :let @"=substitute(system("wl-paste --no-newline"), '<C-v><C-m>', '', 'g')<cr>p


