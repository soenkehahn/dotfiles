local cmd = vim.cmd -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn   -- to call Vim functions e.g. fn.bufnr()
local g = vim.g     -- a table to access global variables
local opt = vim.opt -- to set options

-- leader
g.mapleader = "h"

require("config.lazy")

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
opt.autoread = false

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
map("", "<leader>o", ":call fzf#run({'source': 'fd', 'sink': 'e'})<CR>")

map("", "<leader>m", ":/\\v^[<=>\\|%+]{7}.*$<CR>")

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

map("", "<leader>ü", "<Plug>(coc-cursors-range)")

require('nvim-treesitter').setup {
  highlight = {
    enable = true,
  },
}

require('treesitter-context').setup {
  enable = true,            -- Enable this plugin (Can be enabled/disabled later via commands)
  multiwindow = false,      -- Enable multiwindow support.
  max_lines = 0,            -- How many lines the window should span. Values <= 0 mean no limit.
  min_window_height = 0,    -- Minimum editor window height to enable context. Values <= 0 mean no limit.
  line_numbers = true,
  multiline_threshold = 20, -- Maximum number of lines to show for a single context
  trim_scope = 'outer',     -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
  mode = 'cursor',          -- Line used to calculate context. Choices: 'cursor', 'topline'
  -- Separator between context and content. Should be a single character string, like '-'.
  -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
  separator = '-',
  zindex = 20,     -- The Z-index of the context window
  on_attach = nil, -- (fun(buf: integer): boolean) return false to disable attaching
}

-- -- treesitter stuff
--
-- require('nvim-treesitter.configs').setup {
--   ensure_installed = "all", -- Or specify languages, e.g., {"lua", "python"}
--   highlight = {
--     enable = true,
--     additional_vim_regex_highlighting = false,
--   },
--   playground = {
--     enable = true,
--     updatetime = 25,
--     persist_queries = false,
--   },
-- }
--
-- TSNav = require('treesitter_stuff')
-- -- map("", "<leader>u", TSNav.left)
-- vim.api.nvim_set_keymap("n", "ti", "", {
--   noremap = true,
--   callback = TSNav.to_parent,
-- })
-- vim.api.nvim_set_keymap("n", "te", "", {
--   noremap = true,
--   callback = TSNav.to_child,
-- })
-- vim.api.nvim_set_keymap("n", "tl", "", {
--   noremap = true,
--   callback = TSNav.prev_sibling,
-- })
-- vim.api.nvim_set_keymap("n", "ta", "", {
--   noremap = true,
--   callback = TSNav.next_sibling,
-- })
-- vim.api.nvim_set_keymap("n", "tp", "", {
--   noremap = true,
--   callback = TSNav.from_current_cursor,
-- })
