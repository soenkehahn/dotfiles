// @flow

const child_process = require("child_process");
const fs = require("fs");
const { parseMessages } = require("./parse-messages");

/*::
declare var atom: {|
  workspace: {|
    observeTextEditors: any,
    getActiveTextEditor: any,
    open: (string, ?{
        initialLine?: number,
        initialColumn?: number,
      })
      => void,
  |},
  commands: any,
  views: any,
  project: {
    getPaths: () => Array<string>
  }
|}
*/

const path = require("path");

atom.workspace.observeTextEditors(editor => {
  if (path.basename(editor.getPath()) === "Makefile") {
    editor.setSoftTabs(false);
  }
  if (path.extname(editor.getPath()) === ".mk") {
    editor.setSoftTabs(false);
  }
  if (path.basename(editor.getPath()) === "geany") {
    editor.setSoftTabs(false);
  }
});

atom.commands.add("atom-text-editor", {
  "custom:save-and-exit-insert-mode": event => {
    const editor = atom.workspace.getActiveTextEditor();
    editor.save();
    const editorView = atom.views.getView(editor);
    atom.commands.dispatch(editorView, "vim-mode-plus:activate-normal-mode");
  }
});

atom.commands.add("atom-text-editor", {
  "custom:save-and-test": event => {
    const editor = atom.workspace.getActiveTextEditor();
    editor.save();
    const editorView = atom.views.getView(editor);
    atom.commands.dispatch(editorView, "tertestrial:test-file");
  }
});

// * geany

/*::

type Loc = {|
  file: string,
  line: number,
  col?: number,
|}

*/

let geanMessages /*: Array<Loc> */ = [];
let geanIndex = 0;

atom.commands.add("atom-text-editor", {
  "custom:read-gean": event => {
    const projectDir = atom.project.getPaths()[0];
    const output = fs.readFileSync(projectDir + "/gean", { encoding: "utf-8" });
    geanMessages = parseMessages(output);
    geanIndex = 0;
  }
});

atom.commands.add("atom-text-editor", {
  "custom:next-gean": event => {
    console.log("next-gean");
    if (geanIndex < geanMessages.length) {
      const loc = geanMessages[geanIndex];
      atom.workspace.open(loc.file, {
        initialLine: loc.line - 1,
        initialColumn: loc.col
      });
    }
    geanIndex++;
  }
});
