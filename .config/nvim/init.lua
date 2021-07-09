local cmd = vim.cmd -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn -- to call Vim functions e.g. fn.bufnr()
local g = vim.g -- a table to access global variables
local opt = vim.opt -- to set options

cmd 'packadd paq-nvim' -- load the package manager
require "paq" {
  "savq/paq-nvim"; -- Let Paq manage itself
  "tiagovla/tokyodark.nvim";
}
cmd('PaqSync')
cmd('colorscheme tokyodark')

opt.expandtab = true
opt.shiftwidth = 2
opt.list = true
opt.termguicolors = true
