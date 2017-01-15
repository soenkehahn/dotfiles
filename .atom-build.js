XRegExp = require('xregexp');

module.exports = {
  cmd: "make -f geany",
  functionMatchFoo: function (output) {
    patterns = [
      XRegExp('^(?<file>[a-z\.\/]+):(?<line>\\d+):((?<col>\\d+):)?'),
      XRegExp('^-- .+ -+ (?<file>[a-zA-Z\\/\\.]+)\\n\\n(?<message>.+)\\n\\n( )?(?<line>\\d+)\\|')
    ];
    const locations = [];
    patterns.forEach(pattern => {
      const match = XRegExp.exec(output, pattern);
      if (match) {
        match.line = parseInt(match.line);
        location = {
          file: match.file,
          line: match.line
        };
        if (typeof match.col !== "undefined"){
          location.col = parseInt(match.col);
        }
        locations.push(location);
      }
    });
    return locations;
  }
};
