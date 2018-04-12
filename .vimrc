nnoremap <Leader>d :make doc<CR>

" recognize test error locations
let &errorformat="### Failure in %f:%l: %m," . &errorformat

" highlight haskell blocks in markdown files
let g:markdown_fenced_languages=['haskell']
