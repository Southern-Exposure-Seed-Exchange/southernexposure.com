FROM docker.io/node:14 AS base
RUN apt-get -y update && apt-get install -y libtinfo5 curl

FROM base AS frontend-bulider
WORKDIR /southernexposure
COPY client/package.json ./client/package.json
COPY client/package-lock.json ./client/package-lock.json
RUN npm --prefix client install
COPY client ./client
RUN npm --prefix client run build

