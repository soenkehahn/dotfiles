local cmd = vim.cmd -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn -- to call Vim functions e.g. fn.bufnr()
local g = vim.g -- a table to access global variables
local opt = vim.opt -- to set options

-- options
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

-- plugins
cmd('packadd paq-nvim')
require "paq" {
  "savq/paq-nvim";
  "cespare/vim-toml";
  "akinsho/nvim-bufferline.lua";
  "junegunn/fzf";
  "junegunn/fzf.vim";
  "Yagua/nebulous.nvim";
  "bogado/file-line";
  "vim-autoformat/vim-autoformat";
  "preservim/nerdcommenter";
}

-- leader
g.mapleader = "h"
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- style
cmd('colorscheme nebulous')

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

-- autoformatting
cmd("autocmd BufWrite * :Autoformat")

-- command palette
map("", "<leader>p", ":Commands<CR>")
map("", "<leader>r", ":History:<CR>")

-- commenting
cmd("filetype plugin on")
g.NERDCreateDefaultMappings = 0
g.NERDSpaceDelims = 1
g.NERDDefaultAlign = false
map("", "<leader>k", ":call NERDComment('n', 'Toggle')<CR>")
