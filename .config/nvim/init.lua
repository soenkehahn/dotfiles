local cmd = vim.cmd -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn -- to call Vim functions e.g. fn.bufnr()
local g = vim.g -- a table to access global variables
local opt = vim.opt -- to set options

-- plugins
cmd('packadd paq-nvim')
require "paq" {
  "savq/paq-nvim";
  "akinsho/nvim-bufferline.lua";
  "junegunn/fzf";
  "junegunn/fzf.vim";
  "Yagua/nebulous.nvim";
  "bogado/file-line";
  "vim-autoformat/vim-autoformat";
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

-- other stuff
opt.expandtab = true
opt.shiftwidth = 2
opt.list = true
opt.termguicolors = true
opt.hidden = true
