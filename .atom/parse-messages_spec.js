// @flow

const chai = require('chai');
const expect = chai.expect;
const mocha = require('mocha');
const describe = mocha.describe;
const it = mocha.it;
const child_process = require('child_process');
const _ = require('lodash');

const atomBuild = require('./parse-messages');

function expectMatches(output, expected) {
  const result = atomBuild.parseMessages(output);
  expect(result).to.eql(expected);
}

global.atom = {
  project: {
    getPaths: () => ['./.']
  }
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

describe('parse-messages.js', () => {
  describe('parseMessages', () => {
    describe('error matching', () => {
      it('matches gcc style messages', () => {
        const output = [
          "./file/foo:13:8: message",
          "something"
        ].join('\n');
        const expected = [{
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
        const expected = [{
          file: "./file/foo",
          line: 13
        }];
        expectMatches(output, expected);
      });
      it('matches weird file names', () => {
        const output = "./file/foo-Bar_baz-123.exe:42: message\n";
        const expected = [{
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
        const expected = [{
          file: "file/Operational.elm",
          line: 73
        }];
        expectMatches(output, expected);
      });
      it('matches (uncolorized) hspec messages', () => {
        const output = "  file/foo:85:";
        const expected = [{
          file: "file/foo",
          line: 85
        }];
        expectMatches(output, expected);
      });
      describe('matches mocha test failures', () => {
        _.forEach(['      ', '    '], (spaces) => {
          it(`with ${spaces.length} leading spaces`, () => {
            const output = spaces + "at Context.<anonymous> (file/foo:85:23)";
            const expected = [{
              file: "file/foo",
              line: 85,
              col: 23
            }];
            expectMatches(output, expected);
          });
        });
      });
      it('matches coffeelint errors', () => {
        const output = [
          "  ✗ file/foo",
          "     ✗ #201: Unnecessary double quotes are forbidden.",
        ].join('\n');
        const expected = [{
          file: "file/foo",
          line: 201,
        }];
        expectMatches(output, expected);
      });
      it('matches node exceptions', () => {
        const output = '      at file/foo:26:30';
        const expected = [{
          file: "file/foo",
          line: 26,
          col: 30,
        }];
        expectMatches(output, expected);
      });
      it('matches flow errors', () => {
        const output = 'file/foo:26';
        const expected = [{
          file: "file/foo",
          line: 26,
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
          const expected = [
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
      it('works for absolute files', () => {
        const cwd = process.cwd()
        const output = cwd + "/file/foo.txt:42:\n";
        const expected = [
          {
            file: cwd + "/file/foo.txt",
            line: 42
          }
        ];
        expectMatches(output, expected);
      })
      it("also searches in subdirectory 'tests'", () => {
        const output = "./test.file:42:\n";
        const expected = [
          {
            file: "tests/test.file",
            line: 42
          }
        ];
        expectMatches(output, expected);
      });
      it("also searches in subdirectory 'client/tests'", () => {
        const output = "./client-test.file:42:\n";
        const expected = [
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
