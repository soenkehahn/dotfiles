FROM golang:1.12.5-stretch

RUN apt-get update && apt-get install --yes atool tree

ENV TAG v0.11.13
RUN go get -v -d github.com/hashicorp/terraform
WORKDIR /go/src/github.com/hashicorp/terraform
RUN git checkout $TAG
RUN go get -v
WORKDIR /go
RUN apack /root/result.tar.gz bin
