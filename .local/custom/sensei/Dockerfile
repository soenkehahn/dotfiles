FROM custom-ubuntu-base

WORKDIR /root
ENV TAG 0.4.0
RUN git clone https://github.com/hspec/sensei
WORKDIR sensei
RUN git checkout $TAG
RUN git submodule init
RUN git submodule update

RUN stack init --ignore-subdirs --resolver=lts-9.12
RUN stack setup
RUN stack test
RUN mkdir bin
RUN stack install --local-bin-path bin
RUN apack /root/result.tar.gz bin
