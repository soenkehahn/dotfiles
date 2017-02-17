XRegExp = require('xregexp');
fs = require('fs');
path = require('path');

_functionMatch = function (output) {
  filePattern = '(?<file>[a-zA-Z-_\\d\.\/]+)';
  linePattern = '(?<line>\\d+)';
  patterns = [
    XRegExp(`^ *${filePattern}:${linePattern}:((?<col>\\d+):)?`),
    XRegExp(`^-- .+ -+ ${filePattern}\\n\\n(?<message>.+)\\n\\n( )?${linePattern}\\|`),
    XRegExp(`\\sat Context\.<anonymous> \\(${filePattern}:${linePattern}:(?<col>\\d+)\\)`),
    XRegExp(`  ✗ ${filePattern}\\n     ✗ #${linePattern}: `),
  ];
  var locations = [];
  lines = output.split('\n');
  lines.forEach((line, index) => {
    patterns.forEach(pattern => {
      section = lines.slice(index, index + 5).join('\n');
      const match = XRegExp.exec(section, pattern);
      if (match) {
        match.line = parseInt(match.line);
        loc = {
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
