" set default stuff
set nu
set hidden
"set clipboard=unnamedplus
syntax on
set nofoldenable    " disable folding
" Plugins
call plug#begin()
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'preservim/nerdtree'
" {} colored
Plug 'frazrepo/vim-rainbow'
Plug 'bluz71/vim-nightfly-guicolors'
Plug 'zah/nim.vim'
" autocomplete
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'junegunn/fzf.vim'
" home dashboard
Plug 'glepnir/dashboard-nvim'
" tab bar
Plug 'nvim-lualine/lualine.nvim'
Plug 'kyazdani42/nvim-web-devicons'
" Markdown
Plug 'ellisonleao/glow.nvim', {'branch': 'main'}
Plug 'ziglang/zig.vim'
" theme
Plug 'catppuccin/nvim', {'as': 'catppuccin'}
" Plug 'romgrk/barbar.nvim'
" syntax highlight
cal plug#end()
let g:deoplete#enable_at_startup = 1
let g:rainbow_active = 1
let g:ale_completion_autoimport = 1
let g:dashboard_default_executive ='fzf'
" reverse autocomplete order
let g:SuperTabDefaultCompletionType = "<c-n>"

let g:dashboard_custom_header = [
\ ' ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗',
\ ' ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║',
\ ' ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║',
\ ' ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║',
\ ' ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║',
\ ' ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝',
\]

filetype plugin on

" Enable slim syntax highlight
autocmd FileType slim setlocal foldmethod=indent
autocmd BufNewFile,BufRead *.slim set filetype=slim

" set Colorscheme
set termguicolors
colorscheme catppuccin
" colorscheme nightfly

hi Normal ctermbg=NONE guibg=NONE

" perso command
command Gofmt execute "! gofmt -w %"
"let mapleader = \" \" " map leader to Space
let g:mapleader="\<Space>"
nnoremap <Leader>ff :Telescope find_files<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-s> :w <CR>
nnoremap <C-q> :q <CR>
" for dashboard
nnoremap <silent> <Leader>cn :DashboardNewFile<CR>
nnoremap <silent> <Leader>fh :DashboardFindHistory<CR>
nnoremap <silent> <Leader>tc :DashboardChangeColorscheme<CR>
nnoremap <silent> <Leader>fa :DashboardFindWord<CR>
nnoremap <silent> <Leader>fb :DashboardJumpMark<CR>
" visual mode comment/uncomment line
vmap <C-x> gc <CR>
" Clear highlighting
nnoremap <silent> <cr> :noh<CR><CR>
" change terminal binding to exit
tnoremap <M-Esc> <C-\><C-n>
" glow shortcut
noremap <C-p> :Glow<CR>
" remove number in terminal
autocmd TermOpen * setlocal nonumber norelativenumber

" change tab size to 4 spaces
set shiftwidth=4
set tabstop=4
filetype indent on

" set ! command to bash
set shell=/usr/bin/zsh
set shellcmdflag=-ic

" remove ~ as blank line
set fillchars=eob:\ 

