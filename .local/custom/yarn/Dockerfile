FROM node:8.2.1
RUN apt-get update && apt-get install --yes atool

ENV TAG v1.13.0
WORKDIR /root
RUN git clone https://github.com/yarnpkg/yarn.git
WORKDIR yarn
RUN git checkout $TAG
RUN yarn
RUN yarn run build

RUN rm -rf node_modules
RUN yarn --production

RUN mkdir -p dist/lib/yarn
RUN cp -rv bin/ dist/lib/yarn/bin/
RUN cp -rv lib/ dist/lib/yarn/lib/
RUN cp -rv node_modules/ dist/lib/yarn/node_modules/
RUN cp -v package.json dist/lib/yarn/package.json
RUN mkdir -p dist/bin
RUN cd dist/bin && ln -s ../lib/yarn/bin/yarn
WORKDIR dist
RUN apack /root/result.tar.gz *
