const chai = require('chai');
const expect = chai.expect;

const atomBuild = require('./.atom-build');

describe('.atom-build.js', () => {
  describe('functionMatchFoo', () => {
    it('matches gcc style messages', () => {
      const output = [
        "./file/foo:13:8: message",
        "something"
      ].join('\n');
      result = atomBuild.functionMatchFoo(output);
      expected = [{
        file: "./file/foo",
        line: 13,
        col: 8
      }];
      expect(result).to.eql(expected);
    });
    it('matches gcc style messages without column');
    it('matches weird file names');
    it('matches elm error messages', () => {
      const output = [
        "-- TYPE MISMATCH ------------------------------------- src/Operational/Mocks.elm",
        "",
        "The 4th argument to function `simulate` is causing a mismatch.",
        "",
        "73|                         simulate program",
      ].join('\n');
      // console.log(JSON.stringify(output));
      result = atomBuild.functionMatchFoo(output);
      expected = [{
        file: "src/Operational/Mocks.elm",
        line: 73
      }];
      expect(result).to.eql(expected);
    });
  });
});
