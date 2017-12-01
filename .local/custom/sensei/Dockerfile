FROM ubuntu:17.10
RUN apt-get update && apt-get install --yes atool git haskell-stack zlib1g-dev

WORKDIR /root
ENV TAG 0.3.0
RUN git clone https://github.com/hspec/sensei
WORKDIR sensei
RUN git checkout $TAG
RUN git submodule init
RUN git submodule update

RUN stack init --ignore-subdirs
RUN stack setup
RUN stack test
RUN mkdir bin
RUN stack install --local-bin-path bin
RUN apack /root/result.tar.gz bin
