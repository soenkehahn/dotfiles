local cmd = vim.cmd -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn -- to call Vim functions e.g. fn.bufnr()
local g = vim.g -- a table to access global variables
local opt = vim.opt -- to set options

-- plugins
cmd('packadd paq-nvim')
require "paq" {
  "akinsho/nvim-bufferline.lua";
  "bogado/file-line";
  "cespare/vim-toml";
  "chriskempson/base16-vim";
  "junegunn/fzf";
  "junegunn/fzf.vim";
  "NoahTheDuke/vim-just";
  "tpope/vim-commentary";
  "savq/paq-nvim";
  "sbdchd/neoformat";
}

-- options
opt.clipboard = "unnamedplus"
opt.cursorline = true
opt.expandtab = true
opt.hidden = true
opt.ignorecase = true
opt.list = true
opt.number = true
opt.scrolloff = 10
opt.shiftwidth = 2
opt.smartcase = true
opt.termguicolors = true
g.rust_recommended_style = false

-- leader
g.mapleader = "h"
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
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

-- tabs
require("bufferline").setup{
  options = {
    show_buffer_icons = false,
    show_buffer_close_icons = false,
  },
}
map("", "<leader>l", ":BufferLineCyclePrev<CR>")
map("", "<leader>c", ":BufferLineCycleNext<CR>")
map("", "<leader>w", ":bd<CR>")
map("", "<leader>h", ":w<CR>")
map("", "<leader>a", ":wa<CR>")
map("", "<C-S>", ":w<CR>")

-- opening files
map("", "<leader>o", ":GFiles<CR>")
map("", "<leader>z", ":Ag<CR>")

-- autoformatting
g.autoformat_autoindent = false
cmd([[
augroup fmt
  autocmd!
  autocmd BufWritePre * Neoformat
augroup END
]])

-- command palette
map("", "<leader>p", ":Commands<CR>")
map("", "<leader>r", ":History:<CR>")

-- commenting
cmd("filetype plugin on")
map("", "<leader>k", ":Commentary<CR>")

-- filetypes
cmd("autocmd BufRead,BufNewFile Vagrantfile set filetype=ruby")
cmd("autocmd BufRead,BufNewFile *.justfile set filetype=just")

-- remove search highlighting
map("", "<leader>i", ":noh<CR>")
