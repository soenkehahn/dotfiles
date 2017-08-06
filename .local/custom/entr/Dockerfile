FROM ubuntu:17.04

RUN apt-get update && apt-get install --yes wget atool build-essential

WORKDIR /root
RUN wget http://entrproject.org/code/entr-3.7.tar.gz
RUN aunpack entr-3.7.tar.gz
WORKDIR eradman-entr-c5b62bde107d

RUN ./configure
RUN make test
RUN make install

WORKDIR /usr/local
RUN apack /root/result.tar.gz *
