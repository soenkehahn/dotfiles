
function! GetCursorState()
  let window = winnr()
  let current_buffer = bufnr('%')
  let s_line = line(".")
  let s_col = col(".")
  let s_ctermbg = synIDattr(hlID("ColorColumn"), "bg")
  return [window, current_buffer, s_line, s_col, s_ctermbg]
endfunction

function! SetCursorState(list)
  let window = a:list[0]
  let current_buffer = a:list[1]
  let s_line = a:list[2]
  let s_col = a:list[3]
  let s_ctermbg = a:list[4]

  execute window."wincmd w"
  execute "buffer ".current_buffer
  call cursor(s_line, s_col)
  exec 'highlight ColorColumn ctermbg=' . s_ctermbg
endfunction

function! RestoreCursor(command)
  let state = GetCursorState()
  execute a:command
  call SetCursorState(state)
endfunction
