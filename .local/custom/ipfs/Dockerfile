FROM custom-ubuntu-base

ENV version=0.4.17
WORKDIR /root
RUN wget https://dist.ipfs.io/go-ipfs/v$version/go-ipfs_v${version}_linux-amd64.tar.gz
RUN aunpack go-ipfs_v${version}_linux-amd64.tar.gz
RUN mkdir bin
RUN cp go-ipfs/ipfs bin/
RUN apack /root/result.tar.gz bin

