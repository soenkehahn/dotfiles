XRegExp = require('xregexp');

_functionMatch = function (output) {
  filePattern = '(?<file>[a-zA-Z-_\\d\.\/]+)';
  patterns = [
    XRegExp('^' + filePattern + ':(?<line>\\d+):((?<col>\\d+):)?'),
    XRegExp('^-- .+ -+ ' + filePattern + '\\n\\n(?<message>.+)\\n\\n( )?(?<line>\\d+)\\|')
  ];
  const locations = [];
  lines = output.split('\n');
  lines.forEach((line, index) => {
    patterns.forEach(pattern => {
      section = lines.slice(index, index + 5).join('\n');
      // console.log(JSON.stringify(section));
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
  return locations;
};

module.exports = {
  cmd: "make -f geany",
  functionMatch: _functionMatch,
  _functionMatch: _functionMatch
};
