FROM golang:1.8.3-jessie

RUN apt-get update && apt-get install --yes atool

RUN go get -v github.com/hashicorp/terraform
WORKDIR /go/src/github.com/hashicorp/terraform
RUN git checkout v0.9.11
RUN go get
WORKDIR /go
RUN apack /root/result.tar.gz bin
