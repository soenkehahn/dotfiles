
function! GetCursorState()
  let window = winnr()
  let current_buffer = bufnr('%')
  let s_line = line(".")
  let s_col = col(".")
  return [window, current_buffer, s_line, s_col]
endfunction

function! SetCursorState(list)
  let window = a:list[0]
  let current_buffer = a:list[1]
  let s_line = a:list[2]
  let s_col = a:list[3]

  execute window."wincmd w"
  execute "buffer ".current_buffer
  call cursor(s_line, s_col)
endfunction

function! RestoreCursor(command)
  let state = GetCursorState()
  execute a:command
  call SetCursorState(state)
endfunction
