// @flow

const XRegExp = require('xregexp');
const fs = require('fs');
const path = require('path');

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
  let result = snippets.pop()
  snippets.forEach(snippet => {
    if (! path.isAbsolute(result)) {
      result = snippet + '/' + result
    }
  })
  return path.normalize(result)
}

const parseMessages = function(output /*: string */) /*: Array<Loc> */ {
  const filePattern = '(?<file>[a-zA-Z-_\\d\.\/]+)';
  const linePattern = '(?<line>\\d+)';
  const colPattern = '(?<col>\\d+)';
  const patterns = [
    XRegExp(`^ *${filePattern}:${linePattern}(:(?<col>\\d+):)?`),
    XRegExp(`^-- .+ -+ ${filePattern}\\n\\n(?<message>.+)\\n\\n( )?${linePattern}\\|`),
    XRegExp(`\\sat Context\.<anonymous> \\(${filePattern}:${linePattern}:(?<col>\\d+)\\)`),
    XRegExp(`  ✗ ${filePattern}\\n     ✗ #${linePattern}: `),
    XRegExp(`      at ${filePattern}:${linePattern}:${colPattern}`),
  ];
  var locations = [];
  const lines = output.split('\n');
  lines.forEach((line, index) => {
    patterns.forEach(pattern => {
      const section = lines.slice(index, index + 5).join('\n');
      const match = XRegExp.exec(section, pattern);
      if (match) {
        match.line = parseInt(match.line);
        const loc /*: Loc */ = {
          file: match.file,
          line: match.line,
        };
        if (typeof match.col !== "undefined"){
          loc.col = parseInt(match.col);
        }
        locations.push(loc);
      }
    });
  });
  locations = locations.filter(location => {
    const candidates = [
      location.file,
      combinePaths('tests', location.file),
      combinePaths('client/tests', location.file),
    ]
    const projectDir = atom.project.getPaths()[0]
    let exists = false
    candidates.forEach(candidate => {
      if (fs.existsSync(combinePaths(projectDir, candidate))) {
        location.file = candidate
        exists = true
      }
    })
    if (!exists) {
      console.log('does not exist: ' + location.file);
    }
    return exists;
  });
  return locations;
};

module.exports = {
  parseMessages
};
