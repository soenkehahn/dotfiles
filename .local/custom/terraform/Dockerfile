FROM golang:1.9.1-stretch

RUN apt-get update && apt-get install --yes atool

ENV TAG v0.10.8
RUN go get -v -d github.com/hashicorp/terraform
WORKDIR /go/src/github.com/hashicorp/terraform
RUN git checkout $TAG
RUN go get -v
WORKDIR /go
RUN apack /root/result.tar.gz bin
