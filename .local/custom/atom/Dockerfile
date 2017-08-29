FROM ubuntu:17.04
RUN apt-get update && apt-get install --yes atool wget

ENV VERSION 1.19.4

WORKDIR /root
RUN wget https://github.com/atom/atom/releases/download/v$VERSION/atom-amd64.tar.gz
RUN mkdir -p dist/lib && mkdir -p dist/bin
RUN aunpack atom-amd64.tar.gz --extract-to=dist/lib
RUN cd dist/bin && ln -s ../lib/atom-$VERSION-amd64/atom
RUN cd dist/bin && ln -s ../lib/atom-1.19.4-amd64/resources/app/apm/bin/apm

WORKDIR dist
RUN apack /root/result.tar.gz *
