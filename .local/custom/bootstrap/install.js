#!/usr/bin/env babel-node

// @flow

import uuidv4 from "uuid/v4";

function run(command): void {
  process.stdout.write(`${command}\n`);
}

run("#!/bin/bash");
run("");
run("set -o errexit");
run("set -o xtrace");

function install(dir: string) {
  run("");
  run("");
  run(`true`);
  run(`true ====================================`);
  run(`true installing ${dir}`);
  run(`true ====================================`);
  run(`true`);
  run("");
  const imageName = uuidv4();
  const containerName = uuidv4();

  run(`docker build --tag ${imageName} ${dir}`);
  run(`docker run -it --name ${containerName} ${imageName} true`);
  run(`docker cp ${containerName}:/root/result.tar.gz ${dir}`);
  run(`docker rm ${containerName}`);

  const tarball = `${dir}/result.tar.gz`;
  run(`aunpack --quiet -X ~/.local ${tarball}`);
}

const packages = [
  "atom",
  "blackbox",
  "el",
  "entr",
  "fzf",
  "ngrok",
  "node",
  "sl",
  "terraform",
  "yarn"
];

for (const dir of packages) {
  install(dir);
}
