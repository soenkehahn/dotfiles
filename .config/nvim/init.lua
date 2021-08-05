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
  "preservim/nerdcommenter";
  "savq/paq-nvim";
  "sbdchd/neoformat";
  "Yagua/nebulous.nvim";
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
require("colors")

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
g.NERDCreateDefaultMappings = 0
g.NERDSpaceDelims = 1
g.NERDDefaultAlign = false
map("", "<leader>k", ":call NERDComment('n', 'Toggle')<CR>")

-- filetypes
cmd("autocmd BufRead,BufNewFile Vagrantfile set filetype=ruby")
cmd("autocmd BufRead,BufNewFile *.justfile set filetype=just")

-- remove search highlighting
map("", "<leader>i", ":noh<CR>")
