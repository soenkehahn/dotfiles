FROM ubuntu:17.04

RUN apt-get update && apt-get install --yes wget atool

WORKDIR /root
RUN wget https://bin.equinox.io/c/4VmDzA7iaHb/ngrok-stable-linux-amd64.zip
RUN aunpack ngrok-stable-linux-amd64.zip
RUN mkdir -p dist/bin
RUN cp ngrok dist/bin/

WORKDIR dist
RUN apack /root/result.tar.gz *
