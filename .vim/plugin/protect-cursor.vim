
function! GetCursorState()
  let window = winnr()
  let current_buffer = bufnr('%')
  let s_line = line(".")
  let s_col = col(".")
  let colorColumn_ctermbg = synIDattr(hlID("ColorColumn"), "bg")
  let extraWhitespace_ctermbg = synIDattr(hlID("ExtraWhitespace"), "bg")
  return [window, current_buffer, s_line, s_col, colorColumn_ctermbg, extraWhitespace_ctermbg]
endfunction

function! SetCursorState(list)
  let window = a:list[0]
  let current_buffer = a:list[1]
  let s_line = a:list[2]
  let s_col = a:list[3]
  let colorColumn_ctermbg = a:list[4]
  let extraWhitespace_ctermbg = a:list[5]

  execute window."wincmd w"
  execute "buffer ".current_buffer
  call cursor(s_line, s_col)
  exec 'highlight ColorColumn ctermbg=' . colorColumn_ctermbg
  exec 'highlight ExtraWhitespace ctermbg=' . extraWhitespace_ctermbg
endfunction

function! RestoreCursor(command)
  let state = GetCursorState()
  execute a:command
  call SetCursorState(state)
endfunction
