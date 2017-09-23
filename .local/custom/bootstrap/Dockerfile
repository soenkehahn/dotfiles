FROM node:8.2.1

WORKDIR /root
ADD package.json yarn.lock ./
RUN yarn
ADD .babelrc .flowconfig install.js ./
RUN ./node_modules/.bin/flow

CMD ["./node_modules/.bin/babel-node", "install.js"]
