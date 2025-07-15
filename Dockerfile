FROM docker.io/node:18 AS base
RUN apt-get -y update && apt-get install -y libtinfo5 curl postgresql libpq-dev
RUN curl -sSL https://get.haskellstack.org/ | sh

FROM base AS devcontainer
RUN apt-get -y update && apt-get install -y sudo
RUN curl -sSL https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz | gunzip -c > /usr/local/bin/elm
RUN chmod +x /usr/local/bin/elm
RUN echo "node ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER node
ENV BOOTSTRAP_HASKELL_GHC_VERSION=8.10.7
ENV BOOTSTRAP_HASKELL_INSTALL_HLS=1
# Stack is already installed in the base image, so we don't need to install it again via ghcup
ENV BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1
# The last version of HLS that supports GHC 8.10.7
ENV BOOTSTRAP_HASKELL_HLS_VERSION=2.2.0.0
ENV BOOTSTRAP_HASKELL_ADJUST_BASHRC=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/home/node/.ghcup/bin:/home/node/.cabal/bin:${PATH}"
# To build the frontend
ENV CXXFLAGS=-std=c++17

FROM base AS frontend-builder
WORKDIR /southernexposure
COPY client/package.json ./client/package.json
COPY client/package-lock.json ./client/package-lock.json
ENV CXXFLAGS=-std=c++17
RUN npm --prefix client install
# Actually build client
COPY client ./client
ARG HELCIM_ENV
ENV HELCIM_ENV=$HELCIM_ENV
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

FROM nginx:1.29.0 AS frontend
COPY --from=frontend-builder /southernexposure/client/dist /usr/share/nginx/html
COPY client/nginx/nginx.conf /etc/nginx/conf.d/default.conf
EXPOSE 8080

FROM debian:bookworm-slim AS backend
RUN apt-get -y update && apt-get install -y libpq5
RUN useradd -m -U -d /home/sese -s /bin/bash sese
COPY --from=backend-builder /root/.local/bin/sese-website-exe /usr/local/bin/sese-website-exe
COPY server/scripts/entrypoint.sh /usr/local/bin/entrypoint.sh
EXPOSE 3000
USER sese
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["sese-website-exe"]
