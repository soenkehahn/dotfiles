FROM ubuntu:17.10
RUN apt-get update && apt-get install --yes atool build-essential git libncurses5-dev

WORKDIR /root
RUN git clone https://github.com/mtoyoda/sl
WORKDIR sl
RUN git checkout 5.02
RUN make
RUN mkdir bin
RUN mv sl bin/
RUN apack /root/result.tar.gz bin
