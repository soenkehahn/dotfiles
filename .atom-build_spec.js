const chai = require('chai');
const expect = chai.expect;
const child_process = require('child_process');

const atomBuild = require('./.atom-build');

function expectMatches(output, expected) {
  result = atomBuild._functionMatch(output);
  expect(result).to.eql(expected);
}

process.chdir('/tmp');
child_process.execSync('mkdir -p file');
child_process.execSync('touch file/foo');
child_process.execSync('touch file/foo.txt');
child_process.execSync('touch file/foo-Bar_baz-123.exe');
child_process.execSync('touch file/Operational.elm');
child_process.execSync('mkdir -p tests');
child_process.execSync('touch tests/test.file');
child_process.execSync('mkdir -p client/tests');
child_process.execSync('touch client/tests/client-test.file');

describe('.atom-build.js', () => {
  describe('functionMatchFoo', () => {
    describe('error matching', () => {
      it('matches gcc style messages', () => {
        const output = [
          "./file/foo:13:8: message",
          "something"
        ].join('\n');
        expected = [{
          file: "./file/foo",
          line: 13,
          col: 8
        }];
        expectMatches(output, expected);
      });
      it('matches gcc style messages without column', () => {
        const output = [
          "./file/foo:13: message",
          "something"
        ].join('\n');
        expected = [{
          file: "./file/foo",
          line: 13
        }];
        expectMatches(output, expected);
      });
      it('matches weird file names', () => {
        const output = "./file/foo-Bar_baz-123.exe:42: message\n";
        expected = [{
          file: "./file/foo-Bar_baz-123.exe",
          line: 42
        }];
        expectMatches(output, expected);

      });
      it('matches elm error messages', () => {
        const output = [
          "something",
          "-- TYPE MISMATCH ------------------------------------- file/Operational.elm",
          "",
          "The 4th argument to function `simulate` is causing a mismatch.",
          "",
          "73|                         simulate program",
        ].join('\n');
        expected = [{
          file: "file/Operational.elm",
          line: 73
        }];
        expectMatches(output, expected);
      });
      it('matches (uncolorized) hspec messages', () => {
        const output = "  file/foo:85:";
        expected = [{
          file: "file/foo",
          line: 85
        }];
        expectMatches(output, expected);
      });
      it('matches mocha test failures', () => {
        const output = "      at Context.<anonymous> (file/foo:85:23)";
        expected = [{
          file: "file/foo",
          line: 85,
          col: 23
        }];
        expectMatches(output, expected);
      });
      describe('message order', () => {
        it('matches earlier messages first', () => {
          const output = [
            "something",
            "-- TYPE MISMATCH ------------------------------------- file/Operational.elm",
            "",
            "The 4th argument to function `simulate` is causing a mismatch.",
            "",
            "73|                         simulate program",
            "",
            "./file/foo.txt:42:"
          ].join('\n');
          expected = [
            {
              file: "file/Operational.elm",
              line: 73
            },
            {
              file: "./file/foo.txt",
              line: 42
            },
          ];
          expectMatches(output, expected);
        });
      });
    });
    describe('file locations', () => {
      it('removes file locations of non-existing files', () => {
        const output = "./does-not-exist.file:42:\n"
        expectMatches(output, []);
      });
      it("also searches in subdirectory 'tests'", () => {
        const output = "./test.file:42:\n";
        expected = [
          {
            file: "tests/test.file",
            line: 42
          }
        ];
        expectMatches(output, expected);
      });
      it("also searches in subdirectory 'client/tests'", () => {
        const output = "./client-test.file:42:\n";
        expected = [
          {
            file: "client/tests/client-test.file",
            line: 42
          }
        ];
        expectMatches(output, expected);
      });
    });
  });
});
