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
run("");
run("docker build --tag custom-ubuntu-base custom-ubuntu-base");

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
  run(`rm ${tarball}`);
}

const availablePackages = [
  "ag",
  "blackbox",
  "docker-compose",
  "el",
  "fzf",
  "hindent",
  "ipfs",
  "ngrok",
  "node",
  "sensei",
  "sl",
  "terraform",
  "yarn"
];

const wantedPackages = (() => {
  const args = process.argv.slice(2);
  if (args.length === 0) {
    return availablePackages;
  } else {
    const result = [];
    for (const wantedPackage of args) {
      if (availablePackages.includes(wantedPackage)) {
        result.push(wantedPackage);
      } else {
        const message = [`package not found: ${wantedPackage}`];
        message.push("available packages:");
        for (const availablePackage of availablePackages) {
          message.push(`  ${availablePackage}`);
        }
        console.error(message.join("\n"));
        process.exit(1);
      }
    }
    return result;
  }
})();

for (const dir of wantedPackages) {
  install(dir);
}
