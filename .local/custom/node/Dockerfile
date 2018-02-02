FROM custom-ubuntu-base

ENV version 9.4.0

WORKDIR /root
RUN wget https://nodejs.org/dist/v${version}/node-v${version}-linux-x64.tar.xz
RUN aunpack node-v${version}-linux-x64.tar.xz
WORKDIR node-v${version}-linux-x64
RUN rm CHANGELOG.md LICENSE README.md
RUN apack /root/result.tar.gz *
