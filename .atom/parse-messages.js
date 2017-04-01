// @flow

const XRegExp = require('xregexp');
const fs = require('fs');
const path = require('path');

const _functionMatch = function(output /*: string */) {
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
        const loc /*: any */ = {
          file: match.file,
          line: match.line
        };
        if (typeof match.col !== "undefined"){
          loc.col = parseInt(match.col);
        }
        locations.push(loc);
      }
    });
  });
  locations = locations.filter(location => {
    if (fs.existsSync(location.file)) {
      return true;
    } else if (fs.existsSync('tests/' + location.file)) {
      location.file = path.normalize("tests/" + location.file);
      return true;
    } else if (fs.existsSync('client/tests/' + location.file)) {
      location.file = path.normalize("client/tests/" + location.file);
      return true;
    };
    return false;
  });
  return locations;
};

module.exports = {
  cmd: "make -f geany",
  functionMatch: _functionMatch,
  _functionMatch: _functionMatch
};
