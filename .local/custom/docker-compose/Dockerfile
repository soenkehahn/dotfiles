FROM ubuntu:17.10
RUN apt-get update && apt-get install --yes wget atool

ENV VERSION 1.18.0
WORKDIR /root
RUN wget https://github.com/docker/compose/releases/download/$VERSION/docker-compose-Linux-x86_64
RUN chmod +x docker-compose-Linux-x86_64
RUN mkdir -p dist/bin
RUN cp docker-compose-Linux-x86_64 dist/bin/docker-compose

WORKDIR dist
RUN apack /root/result.tar.gz *
