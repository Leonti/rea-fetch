FROM mesosphere/aws-cli
#FROM alpine:3.3

RUN apk add --no-cache --update curl

WORKDIR /root

COPY .stack-work/install/x86_64-linux/lts-7.2/8.0.1/bin/rea-fetch rea-fetch
#RUN apk add --update curl && \
#    rm -rf /var/cache/apk/*

#https://github.com/dkubb/haskell-builder
#https://www.reddit.com/r/haskell/comments/3kjpwe/how_to_easily_create_portable_binaries_for_linux/

ENTRYPOINT ["/bin/sh", "-c"]
