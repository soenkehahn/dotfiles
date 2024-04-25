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
  "neovim/nvim-lspconfig";
  "NoahTheDuke/vim-just";
  "savq/paq-nvim";
  "sbdchd/neoformat";
  "tpope/vim-commentary";
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

-- lsp stuff

local lspconfig = require('lspconfig')
lspconfig.rust_analyzer.setup {}
lspconfig.hls.setup {}
lspconfig.tsserver.setup {}

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', '<leader>g', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', '<leader>d', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', '<leader>e', ":cclose<CR>", opts)
    vim.keymap.set('n', '<leader>h', function()
      if isQfOpen() then
        vim.cmd("cnext")
      else
        vim.diagnostic.setqflist({
          severity = vim.diagnostic.severity.ERROR
        })
        vim.cmd("exec \"normal \\<CR>\"")
      end
    end, opts)
  end,
})

function isQfOpen()
  local openWindows = vim.call('getwininfo')
  for i,v in ipairs(openWindows) do
    if v.quickfix == 1 then
      return true
    end
  end
  return false
end

function dbg(o)
  print(dump(o))
end

function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end
