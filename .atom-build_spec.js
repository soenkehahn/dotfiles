const chai = require('chai');
const expect = chai.expect;

const atomBuild = require('./.atom-build');

function expectMatches(output, expected) {
  result = atomBuild.functionMatchFoo(output);
  expect(result).to.eql(expected);
}

describe('.atom-build.js', () => {
  describe('functionMatchFoo', () => {
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
    it('matches weird file names');
    it('matches elm error messages', () => {
      const output = [
        "-- TYPE MISMATCH ------------------------------------- src/Operational/Mocks.elm",
        "",
        "The 4th argument to function `simulate` is causing a mismatch.",
        "",
        "73|                         simulate program",
      ].join('\n');
      expected = [{
        file: "src/Operational/Mocks.elm",
        line: 73
      }];
      expectMatches(output, expected);
    });
  });
});
