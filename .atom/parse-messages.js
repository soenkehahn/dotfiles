// @flow

const XRegExp = require("xregexp");
const fs = require("fs");
const path = require("path");

/*::

declare var atom: {|
  project: {
    getPaths: () => Array<string>
  },
|}

type Loc = {|
  file: string,
  line: number,
  col?: number,
|}
*/

function combinePaths(...snippets /*: Array<string> */) /*: string */ {
  let result = snippets.pop();
  snippets.forEach(snippet => {
    if (!path.isAbsolute(result)) {
      result = snippet + "/" + result;
    }
  });
  return path.normalize(result);
}

const parseMessages = function(messages /*: string */) /*: Array<Loc> */ {
  const filePattern = "(?<file>[a-zA-Z-_\\d./]+)";
  const linePattern = "(?<line>\\d+)";
  const colPattern = "(?<col>\\d+)";
  const patterns = [
    `^\u001B[^ ]* *${filePattern}:${linePattern}: `,
    `^ *${filePattern}:${linePattern}(:${colPattern}:)?`,
    `^Error: ${filePattern}:${linePattern}`,
    `^(Error|Warning) -* ${filePattern}:${linePattern}:${colPattern}`,
    `^-- .+ -+ ${filePattern}\\n\\n(?<message>.+)\\n\\n( )?${linePattern}\\|`,
    `\\sat Context\.<anonymous> \\(${filePattern}:${linePattern}:(?<col>\\d+)\\)`,
    `  ✗ ${filePattern}\\n     ✗ #${linePattern}: `,
    `      at ${filePattern}:${linePattern}:${colPattern}`,
    `${filePattern}\\n  ( )?${linePattern}:${colPattern}  `,
    `\u001b\\[0m +\u001b\\[0m\u001b\\[0m\u001b\\[1m\u001b\\[38;5;12m--> \u001b\\[0m\u001b\\[0m${filePattern}:${linePattern}:${colPattern}\u001b\\[0m`
  ].map(pattern => new XRegExp(pattern));
  var locations = [];
  const lines = messages.split("\n");
  lines.forEach((line, index) => {
    patterns.forEach(pattern => {
      const section = lines.slice(index, index + 5).join("\n");
      const match = XRegExp.exec(section, pattern);
      if (match) {
        match.line = parseInt(match.line);
        const loc /*: Loc */ = {
          file: match.file,
          line: match.line
        };
        if (typeof match.col !== "undefined") {
          loc.col = parseInt(match.col) - 1;
        }
        locations.push(loc);
      }
    });
  });
  locations = locations.filter(location => {
    const candidates = [
      location.file,
      combinePaths("tests", location.file),
      combinePaths("client/tests", location.file)
    ];
    const projectDir = atom.project.getPaths()[0];
    let exists = false;
    candidates.forEach(candidate => {
      if (fs.existsSync(combinePaths(projectDir, candidate))) {
        location.file = candidate;
        exists = true;
      }
    });
    if (!exists) {
      console.log("does not exist: " + location.file);
    }
    return exists;
  });
  return locations;
};

module.exports = {
  parseMessages
};
