rest_users
=====

OTP application as REST API service

Test
-----

    $ rebar3 ct

Build
-----

    $ rebar3 compile

Deploy
-----

    $ docker compose up

SSL keys
-----
    ```
    openssl req -new -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 \
     -x509 -days 365 -nodes -subj "/C=US/ST=Denial/L=Springfield/O=Dis/CN=www.example.com" \
     -keyout ./priv/ssl/server.key -out ./priv/ssl/server.pem
    ```