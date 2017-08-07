#!/usr/bin/env babel-node

// @flow

import { execSync } from "child_process";
import uuidv4 from "uuid/v4";

function run(command) {
  console.log(command);
  execSync(command, { stdio: "inherit" });
}

run("flow");

function install(dir: string) {
  const imageName = uuidv4();
  const containerName = uuidv4();

  run(`docker build --tag ${imageName} ${dir}`);
  run(`docker run -it --name ${containerName} ${imageName} true`);
  run(`docker cp ${containerName}:/root/result.tar.gz ${dir}`);
  run(`docker rm ${containerName}`);

  const tarball = `${dir}/result.tar.gz`;
  console.error("installing this into /usr/local:");
  run(`als ${tarball}`);
  run(`aunpack -X ~/.local ${tarball}`);
}
install("entr");
install("terraform");
install("node");
install("el");
install("sl");
