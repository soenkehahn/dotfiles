#!/usr/bin/env babel-node

// @flow

import { execSync } from "child_process";
import uuidv4 from "uuid/v4";

function run(command) {
  console.log(command);
  execSync(command, { stdio: "inherit" });
}

const imageName = uuidv4();
const containerName = uuidv4();

run(`docker build --tag ${imageName} .`);
run(`docker run -it --name ${containerName} ${imageName} true`);
run(`docker cp ${containerName}:/root/result.tar.gz .`);
run(`docker rm ${containerName}`);

console.error("installing this into /usr/local:");
run(`als result.tar.gz`);
run(`sudo aunpack -X /usr/local result.tar.gz`);
