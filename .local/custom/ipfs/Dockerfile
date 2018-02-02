FROM custom-ubuntu-base

WORKDIR /root
RUN wget https://dist.ipfs.io/go-ipfs/v0.4.13/go-ipfs_v0.4.13_linux-amd64.tar.gz
RUN aunpack go-ipfs_v0.4.13_linux-amd64.tar.gz
RUN mkdir bin
RUN cp go-ipfs/ipfs bin/
RUN apack /root/result.tar.gz bin

