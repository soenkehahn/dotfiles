FROM custom-ubuntu-base

ENV TAG v1.20170611

WORKDIR /root
RUN git clone https://github.com/StackExchange/blackbox.git
WORKDIR blackbox
RUN git checkout $TAG
RUN mkdir -p dist/bin
RUN cp bin/* dist/bin/
RUN rm dist/bin/Makefile
WORKDIR dist
RUN apack /root/result.tar.gz *
