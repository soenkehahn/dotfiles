FROM ubuntu:16.04
RUN apt-get update && apt-get install --yes atool git haskell-stack zlib1g-dev build-essential

WORKDIR /root
ENV TAG 5.2.4
RUN git clone https://github.com/commercialhaskell/hindent.git

WORKDIR hindent
RUN git checkout $TAG

RUN stack setup
RUN mkdir bin
RUN stack install --local-bin-path bin
RUN apack /root/result.tar.gz bin
