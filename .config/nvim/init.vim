set number					" line numbers
set relativenumber				" relative line numbers
syntax on					" enables syntax highlights

set ignorecase					" ingores case when searching

set complete+=kspell				" auto complete with spellcheck
set completeopt=menuone				" auto complete with menu

call plug#begin('~/.config/nvim/plugged')
Plug 'junegunn/vim-easy-align'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'dylanaraps/wal.vim'

Plug 'ap/vim-css-color'

Plug 'arcticicestudio/nord-vim'

Plug 'dracula/vim',{'ac':'dracula'}

call plug#end()

colorscheme dracula
