FROM ubuntu:17.10

RUN apt-get update && apt-get install --yes wget atool build-essential git automake pkg-config libpcre3-dev zlib1g-dev liblzma-dev tree

RUN git clone https://github.com/ggreer/the_silver_searcher
WORKDIR the_silver_searcher
RUN git checkout 2.0.0
RUN ./build.sh
RUN make install

WORKDIR /usr/local
RUN apack /root/result.tar.gz *
