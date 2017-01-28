path = require 'path'

# Set Makefile[.any extension] and '.mk' files to always use hard tabs
atom.workspace.observeTextEditors (editor) ->
  if path.basename(editor.getPath()) is 'Makefile'
    editor.setSoftTabs(false)
  if path.extname(editor.getPath()) is '.mk'
    editor.setSoftTabs(false)
  if path.basename(editor.getPath()) is 'geany'
    editor.setSoftTabs(false)

atom.commands.add 'atom-text-editor',
  "custom:save-and-exit-insert-mode": (event) ->
    editor = atom.workspace.getActiveTextEditor()
    editor.save()
    editorView = atom.views.getView(editor)
    atom.commands.dispatch editorView, "vim-mode-plus:activate-normal-mode"
