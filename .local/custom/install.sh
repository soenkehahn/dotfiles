#!/bin/bash

set -o errexit
set -o xtrace

# creating installation script
docker build --tag custom-bootstrap:latest bootstrap
docker run --rm custom-bootstrap:latest > install.generated.sh

bash install.generated.sh
