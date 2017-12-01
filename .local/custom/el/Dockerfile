FROM fpco/stack-build:lts-5.9
RUN apt-get update && apt-get install --yes atool

WORKDIR /root
RUN git clone https://github.com/soenkehahn/el.git
WORKDIR el
RUN stack test
RUN mkdir bin
RUN stack install --local-bin-path bin
RUN apack /root/result.tar.gz bin
