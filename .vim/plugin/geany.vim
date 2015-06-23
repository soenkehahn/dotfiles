
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

let g:geany_qf_opened = 0
function! Geany()
  let state = GetCursorState()

  wa
  silent make!
  let g:geany_qf_opened = 1
  if ! IsQuickfixOpen()
    copen
    AnsiEsc
    wincmd L
  endif

  call SetCursorState(state)

  " go to the bottom of the quickfix list
  wincmd l
  normal G
  AnsiEsc!
  wincmd h

  redraw!
endfunction

function! GeanyNext()
  if g:geany_qf_opened
    crewind
    let g:geany_qf_opened = 0
  endif
  cnext
endfunction
