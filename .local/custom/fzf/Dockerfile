FROM golang:1.8.3-stretch
RUN apt-get update && apt-get install --yes atool

ENV TAG 0.17.0-2
WORKDIR /root
RUN go get -v -d github.com/junegunn/fzf
WORKDIR /go/src/github.com/junegunn/fzf
RUN git checkout $TAG
RUN go get -v
RUN mkdir -p /go/lib/fzf
RUN cp shell/*.zsh /go/lib/fzf/
RUN cp bin/fzf-tmux /go/bin
WORKDIR /go

RUN apack /root/result.tar.gz bin lib
RUN als /root/result.tar.gz
