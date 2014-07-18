
" currently not used
function! GetQuickfixBuffer()
  let idx = 1
  let quickfix_buffer = -1
  while idx <= bufnr("$")
    let buffer_type = getbufvar(idx, '&buftype')
    if buffer_type ==# 'quickfix'
      let quickfix_buffer = idx
    endif
    let idx += 1
  endwhile
  return quickfix_buffer
endfunction

function! IsQuickfixOpen()
  for window in range(1, winnr('$'))
    let buffer_type = getwinvar(window, '&buftype')
    if buffer_type ==# 'quickfix'
      return 1
    endif
  endfor
  return 0
endfunction

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

function! Geany()
  let state = GetCursorState()

  wa
  silent make
  let g:geany_qf_opened = 1
  if ! IsQuickfixOpen()
    copen
    wincmd L
  endif

  call SetCursorState(state)

  redraw!
endfunction
