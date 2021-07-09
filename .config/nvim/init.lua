local cmd = vim.cmd -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn -- to call Vim functions e.g. fn.bufnr()
local g = vim.g -- a table to access global variables
local opt = vim.opt -- to set options

g.mapleader = "h"
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

cmd 'packadd paq-nvim' -- load the package manager
require "paq" {
  "savq/paq-nvim"; -- Let Paq manage itself
  "tiagovla/tokyodark.nvim";
  "kyazdani42/nvim-web-devicons";
  "akinsho/nvim-bufferline.lua";
  "junegunn/fzf";
  "junegunn/fzf.vim";
}
cmd('PaqSync')

-- style
cmd('colorscheme tokyodark')

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

-- other stuff
opt.expandtab = true
opt.shiftwidth = 2
opt.list = true
opt.termguicolors = true
