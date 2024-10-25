FROM fpco/stack-build:lts-8.23 AS builder

RUN apt-get update && apt-get install -y expect

WORKDIR /workdir

COPY silly-joy.cabal stack.yaml .
RUN stack install --only-dependencies --test

COPY README.md LICENSE .

COPY src src
COPY app app
RUN stack build

COPY test test
RUN stack test --dump-logs

RUN stack --local-bin-path /sbin install

COPY repl.expect .
RUN ./repl.expect /sbin/silly-joy-exe

FROM alpine:3.3
RUN apk update && apk add ncurses-terminfo-base
COPY --from=builder /sbin/silly-joy-exe /silly-joy-exe
ENTRYPOINT ["/silly-joy-exe"]
