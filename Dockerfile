FROM erlang:21.0.1 as builder

WORKDIR /src
COPY . /src
RUN rebar3 as docker tar

RUN mkdir -p /release
RUN tar -zxvf /src/_build/docker/rel/*/*.tar.gz -C /release

FROM debian:stretch-slim

RUN apt-get update && apt-get install -y openssl daemontools

WORKDIR /deploy

COPY --from=builder /release /deploy

ADD flamegraph /usr/bin

EXPOSE 80
EXPOSE 8192

CMD /deploy/bin/aflame foreground
