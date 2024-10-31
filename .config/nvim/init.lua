local cmd = vim.cmd -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn   -- to call Vim functions e.g. fn.bufnr()
local g = vim.g     -- a table to access global variables
local opt = vim.opt -- to set options

-- plugins
cmd('packadd paq-nvim')
require "paq" {
  "akinsho/nvim-bufferline.lua",
  -- "bogado/file-line",
  -- "cespare/vim-toml",
  "chriskempson/base16-vim",
  "junegunn/fzf",
  "junegunn/fzf.vim",
  -- "neovim/nvim-lspconfig",
  "savq/paq-nvim",
  -- "nvim-tree/nvim-web-devicons",
  "tpope/vim-commentary",
  { "neoclide/coc.nvim", branch = "release" },
}

-- options
opt.clipboard = "unnamedplus"
-- opt.cursorline = true
opt.expandtab = true
-- opt.hidden = true
opt.ignorecase = true
opt.list = true
opt.number = true
-- opt.scrolloff = 10
opt.shiftwidth = 2
opt.smartcase = true
opt.termguicolors = true
-- g.rust_recommended_style = false

-- leader
g.mapleader = "h"
local function map(mode, lhs, rhs, opts)
  local options = { noremap = true }
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- style
pcall(require, "colors")
cmd([[
augroup on_change_colorschema
  autocmd!
  autocmd ColorScheme * highlight CursorLine cterm=NONE ctermbg=NONE ctermfg=NONE guibg=NONE guifg=NONE
augroup END
]])
cmd(":source ~/.config/nvim/colors.vim")

-- tabs
require("bufferline").setup {
  options = {
    show_buffer_icons = false,
    show_buffer_close_icons = false,
  },
}
map("", "<leader>l", ":BufferLineCyclePrev<CR>")
map("", "<leader>c", ":BufferLineCycleNext<CR>")
map("", "<leader>3", ":BufferLineMovePrev<CR>")
map("", "<leader>4", ":BufferLineMoveNext<CR>")
map("", "<leader>w", ":bd<CR>")
map("", "<leader>a", ":wa<CR>")
map("", "<C-S>", ":call CocAction('format')<CR>:w<CR>")

-- opening files
map("", "<leader>o", ":GFiles<CR>")
-- map("", "<leader>z", ":Ag<CR>")

map("", "<leader>m", ":/\\v^[<=>\\|]{7}.*$<CR>")

-- command palette
map("", "<leader>p", ":Commands<CR>")
map("", "<leader>r", ":History:<CR>")

-- commenting
cmd("filetype plugin on")
map("", "<leader>k", ":Commentary<CR>")
cmd("autocmd FileType nix setlocal commentstring=#\\ %s")
cmd("autocmd FileType sql setlocal commentstring=--\\ %s")

-- remove search highlighting
map("", "<leader>i", ":noh<CR>")

-- coc stuff

map("", "<leader>h", "<Plug>(coc-diagnostic-next)")
map("", "<leader>g", "<Plug>(coc-definition)")
map("", "<leader>f", "<Plug>(coc-references)")
function _G.show_docs()
  local cw = vim.fn.expand('<cword>')
  if vim.fn.index({ 'vim', 'help' }, vim.bo.filetype) >= 0 then
    vim.api.nvim_command('h ' .. cw)
  elseif vim.api.nvim_eval('coc#rpc#ready()') then
    vim.fn.CocActionAsync('doHover')
  else
    vim.api.nvim_command('!' .. vim.o.keywordprg .. ' ' .. cw)
  end
end

map("", "<leader>d", "<CMD>lua _G.show_docs()<CR>")
map("", "<leader>.", "<Plug>(coc-codeaction)")
map("", "<leader>t", "<Plug>(coc-git-nextchunk)")
