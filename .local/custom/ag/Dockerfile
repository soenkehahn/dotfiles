FROM custom-ubuntu-base

RUN git clone https://github.com/ggreer/the_silver_searcher
WORKDIR the_silver_searcher
RUN git checkout 2.0.0
RUN ./build.sh
RUN make install

WORKDIR /usr/local
RUN apack /root/result.tar.gz *
