# Build stage 0
FROM erlang:25-alpine

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy Erlang application
COPY . .

# Build the release
RUN rebar3 release

# Build stage 1
FROM alpine:3.16

    # write files generated during startup to /tmp
ENV RELX_OUT_FILE_PATH=/tmp \ 
    # TODO: share log
    REST_USERS_LOGFILE=/tmp/log/error.log

# Install libs
RUN apk add --update --no-cache openssl && \
    apk add --update --no-cache ncurses-libs && \
    apk add --update --no-cache libstdc++

# Install the released application
COPY --from=0 /buildroot/_build/default/rel/ /app

# Expose relevant port
EXPOSE 8443

ENTRYPOINT ["/app/rest_users/bin/rest_users"]
CMD ["foreground"]
