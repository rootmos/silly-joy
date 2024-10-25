FROM fpco/stack-build:lts-8.23 as builder

RUN mkdir /silly-joy
WORKDIR /silly-joy

ADD silly-joy.cabal .
ADD stack.yaml .

ADD src src
ADD test test
ADD app app
ADD README.md .
ADD LICENSE .

RUN stack setup
RUN stack --local-bin-path /sbin install --test

RUN apt-get update && apt-get install -y expect
ADD repl.expect .
RUN ./repl.expect /sbin/silly-joy-exe

FROM alpine:3.3
WORKDIR /root
COPY --from=builder /sbin/silly-joy-exe .
RUN apk update && apk add ncurses-terminfo-base
ENTRYPOINT ["./silly-joy-exe"]
