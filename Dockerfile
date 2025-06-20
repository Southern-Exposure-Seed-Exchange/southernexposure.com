FROM docker.io/node:14 AS base
RUN apt-get -y update && apt-get install -y libtinfo5 curl postgresql libpq-dev
# Stack resolver is very old, so we have to use an older version of stack
RUN curl -sSL https://github.com/commercialhaskell/stack/releases/download/v2.11.1/stack-2.11.1-linux-x86_64.tar.gz \
  | tar -xz -C /usr/local/bin --strip-components=1

FROM base AS frontend-bulider
WORKDIR /southernexposure
COPY client/package.json ./client/package.json
COPY client/package-lock.json ./client/package-lock.json
RUN npm --prefix client install
# Actually build client
COPY client ./client
RUN npm --prefix client run build

FROM base AS backend-builder
WORKDIR /southernexposure
COPY server/stack.yaml ./server/stack.yaml
COPY server/stack.yaml.lock ./server/stack.yaml.lock
COPY server/package.yaml ./server/package.yaml
WORKDIR /southernexposure/server
RUN stack build --ghc-options="-Werror -O1" --only-dependencies sese-website
# Actually build server
COPY server .
RUN stack build --ghc-options="-Werror -O1" --copy-bins 