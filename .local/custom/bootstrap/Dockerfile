FROM node:8.2.1

WORKDIR /root
ADD package.json yarn.lock ./
RUN yarn
ADD .babelrc .flowconfig install.js ./
RUN ./node_modules/.bin/flow
ADD argv.generated ./

CMD ["bash", "-c", "cat argv.generated | xargs ./node_modules/.bin/babel-node install.js"]
