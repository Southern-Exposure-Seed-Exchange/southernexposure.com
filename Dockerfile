FROM docker.io/node:14 AS base
RUN apt-get -y update && apt-get install -y libtinfo5 curl postgresql libpq-dev
RUN curl -sSL https://get.haskellstack.org/ | sh

FROM base AS devcontainer
RUN apt-get -y update && apt-get install -y sudo
RUN echo "node ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

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
# TODO sand-witch: remove -Wno-deprecations
RUN stack build --ghc-options="-Werror -Wno-deprecations -O1" --copy-bins
