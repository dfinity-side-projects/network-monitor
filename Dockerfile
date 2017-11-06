FROM ubuntu:xenial

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get upgrade
RUN apt-get update
RUN apt-get install -y \
	curl \
	git \
	wget \
	nodejs \
	npm

RUN ln -s $(which nodejs) /usr/bin/node

RUN curl -sSL https://get.haskellstack.org | sh
RUN stack --resolver lts-9.9 setup

ADD . /workspace
WORKDIR /workspace

RUN stack build

ENTRYPOINT ./run.sh