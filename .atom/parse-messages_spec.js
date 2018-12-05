// @flow

const chai = require("chai");
const expect = chai.expect;
const mocha = require("mocha");
const describe = mocha.describe;
const it = mocha.it;
const child_process = require("child_process");
const _ = require("lodash");

const atomBuild = require("./parse-messages");

function expectMatches(output, expected) {
  const result = atomBuild.parseMessages(output);
  expect(result).to.eql(expected);
}

global.atom = {
  project: {
    getPaths: () => ["./."]
  }
};

process.chdir("/tmp");
child_process.execSync("mkdir -p file");
child_process.execSync("touch file/foo");
child_process.execSync("touch file/foo.js");
child_process.execSync("touch file/foo.txt");
child_process.execSync("touch file/foo-Bar_baz-123.exe");
child_process.execSync("touch file/Operational.elm");
child_process.execSync("mkdir -p tests");
child_process.execSync("touch tests/test.file");
child_process.execSync("mkdir -p client/tests");
child_process.execSync("touch client/tests/client-test.file");

describe("parse-messages.js", () => {
  describe("parseMessages", () => {
    describe("error matching", () => {
      it("matches gcc style messages", () => {
        const output = ["./file/foo:13:8: message", "something"].join("\n");
        const expected = [
          {
            file: "./file/foo",
            line: 13,
            col: 7
          }
        ];
        expectMatches(output, expected);
      });

      it("matches gcc style messages without column", () => {
        const output = ["./file/foo:13: message", "something"].join("\n");
        const expected = [
          {
            file: "./file/foo",
            line: 13
          }
        ];
        expectMatches(output, expected);
      });

      it("matches weird file names", () => {
        const output = "./file/foo-Bar_baz-123.exe:42: message\n";
        const expected = [
          {
            file: "./file/foo-Bar_baz-123.exe",
            line: 42
          }
        ];
        expectMatches(output, expected);
      });

      it("matches elm error messages", () => {
        const output = [
          "something",
          "-- TYPE MISMATCH ------------------------------------- file/Operational.elm",
          "",
          "The 4th argument to function `simulate` is causing a mismatch.",
          "",
          "73|                         simulate program"
        ].join("\n");
        const expected = [
          {
            file: "file/Operational.elm",
            line: 73
          }
        ];
        expectMatches(output, expected);
      });

      it("matches (uncolorized) hspec messages", () => {
        const output = "  file/foo:85:";
        const expected = [
          {
            file: "file/foo",
            line: 85
          }
        ];
        expectMatches(output, expected);
      });

      it("matches colorized hspec messages", () => {
        const output = "\n\u001b[36m  file/foo:30: \n";
        const expected = [
          {
            file: "file/foo",
            line: 30
          }
        ];
        expectMatches(output, expected);
      });

      describe("matches mocha test failures", () => {
        _.forEach(["      ", "    "], spaces => {
          it(`with ${spaces.length} leading spaces`, () => {
            const output = spaces + "at Context.<anonymous> (file/foo:85:23)";
            const expected = [
              {
                file: "file/foo",
                line: 85,
                col: 22
              }
            ];
            expectMatches(output, expected);
          });
        });
      });

      it("matches coffeelint errors", () => {
        const output = [
          "  ✗ file/foo",
          "     ✗ #201: Unnecessary double quotes are forbidden."
        ].join("\n");
        const expected = [
          {
            file: "file/foo",
            line: 201
          }
        ];
        expectMatches(output, expected);
      });

      it("matches node exceptions", () => {
        const output = "      at file/foo:26:30";
        const expected = [
          {
            file: "file/foo",
            line: 26,
            col: 29
          }
        ];
        expectMatches(output, expected);
      });

      describe("flow error messages", () => {
        it("matches old flow errors", () => {
          const output = "Error: file/foo.js:47";
          const expected = [
            {
              file: "file/foo.js",
              line: 47
            }
          ];
          expectMatches(output, expected);
        });
        it("matches version 0.66 flow errors", () => {
          const output =
            "Error -------------------------------------------------------------------------------------------- file/foo.js:3:10\n";
          const expected = [
            {
              file: "file/foo.js",
              line: 3,
              col: 9
            }
          ];
          expectMatches(output, expected);
        });
        it("matches version 0.66 flow warnings", () => {
          const output =
            "Warning -------------------------------------------------------------------------------------------- file/foo.js:3:10\n";
          const expected = [
            {
              file: "file/foo.js",
              line: 3,
              col: 9
            }
          ];
          expectMatches(output, expected);
        });
      });

      describe("eslint messages", () => {
        it("matches eslint errors", () => {
          const output = [
            "file/foo.js",
            "  47:8  error  'result' is assigned a value but never used  no-unused-vars"
          ].join("\n");
          const expected = [
            {
              file: "file/foo.js",
              line: 47,
              col: 7
            }
          ];
          expectMatches(output, expected);
        });

        it("matches eslint errors with single digit line numbers", () => {
          const output = [
            "file/foo.js",
            "   7:8  error  'result' is assigned a value but never used  no-unused-vars"
          ].join("\n");
          const expected = [
            {
              file: "file/foo.js",
              line: 7,
              col: 7
            }
          ];
          expectMatches(output, expected);
        });
      });

      describe("message order", () => {
        it("matches earlier messages first", () => {
          const output = [
            "something",
            "-- TYPE MISMATCH ------------------------------------- file/Operational.elm",
            "",
            "The 4th argument to function `simulate` is causing a mismatch.",
            "",
            "73|                         simulate program",
            "",
            "./file/foo.txt:42:"
          ].join("\n");
          const expected = [
            {
              file: "file/Operational.elm",
              line: 73
            },
            {
              file: "./file/foo.txt",
              line: 42
            }
          ];
          expectMatches(output, expected);
        });
      });

      describe("rustc", () => {
        it("matches rustc error messages", () => {
          const output =
            "\u001b[0m \u001b[0m\u001b[0m\u001b[1m\u001b[38;5;12m--> \u001b[0m\u001b[0mfile/foo.js:8:5\u001b[0m";
          const expected = [
            {
              file: "file/foo.js",
              line: 8,
              col: 4
            }
          ];
          expectMatches(output, expected);
        });

        it("matches rustc error messages with multiple spaces", () => {
          const output =
            "\u001b[0m   \u001b[0m\u001b[0m\u001b[1m\u001b[38;5;12m--> \u001b[0m\u001b[0mfile/foo.js:8:5\u001b[0m";
          const expected = [
            {
              file: "file/foo.js",
              line: 8,
              col: 4
            }
          ];
          expectMatches(output, expected);
        });

        it("matches errors with newlines at the end", () => {
          const output =
            "\u001b[0m   \u001b[0m\u001b[0m\u001b[1m\u001b[38;5;12m--> \u001b[0m\u001b[0mfile/foo.js:8:5\u001b[0m\n";
          const expected = [
            {
              file: "file/foo.js",
              line: 8,
              col: 4
            }
          ];
          expectMatches(output, expected);
        });
      });

      describe("makam", () => {
        it("matches errors with character ranges", () => {
          const output =
            "!! Error in file ./file/foo.js, line 8, characters 5-7:";
          const expected = [
            {
              file: "./file/foo.js",
              line: 8,
              col: 4
            }
          ];
          expectMatches(output, expected);
        });

        it("matches errors with one character position", () => {
          const output =
            "!! Error in file ./file/foo.js, line 14, character 5:";
          const expected = [
            {
              file: "./file/foo.js",
              line: 14,
              col: 4
            }
          ];
          expectMatches(output, expected);
        });
      });

      describe("tslint messages", () => {
        it("matches tslint errors", () => {
          const output =
            "ERROR: ./file/foo.js[36, 17]: expected call-signature: 'foo' to have a typedef";
          const expected = [
            {
              file: "./file/foo.js",
              line: 36,
              col: 16
            }
          ];
          expectMatches(output, expected);
        });
      });
    });

    describe("file locations", () => {
      it("removes file locations of non-existing files", () => {
        const output = "./does-not-exist.file:42:\n";
        expectMatches(output, []);
      });

      it("works for absolute files", () => {
        const cwd = process.cwd();
        const output = cwd + "/file/foo.txt:42:\n";
        const expected = [
          {
            file: cwd + "/file/foo.txt",
            line: 42
          }
        ];
        expectMatches(output, expected);
      });

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
