FROM golang:1.8.3-jessie

RUN apt-get update && apt-get install --yes atool

RUN go get -v -d github.com/hashicorp/terraform
WORKDIR /go/src/github.com/hashicorp/terraform
RUN go get -v
RUN git checkout v0.9.11
RUN go get -v
WORKDIR /go
RUN apack /root/result.tar.gz bin
