%%%-------------------------------------------------------------------
%% @doc rest_users public API
%% @end
%%%-------------------------------------------------------------------

-module(rest_users_app).

-behaviour(application).

-export([
    start/2
    ,stop/1
]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", toppage_h, []},
            {"/users", users_h, []},
            {"/auth", auth_h, []}
        ]}
    ]),
    case code:priv_dir(rest_users) of
        {error, bad_name} ->
            PrivDir = "priv";
        PrivDir ->
            ok
        end,
    % generate SSL keys:
    % openssl req -new -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 \
    %  -x509 -days 365 -nodes -subj "/C=US/ST=Denial/L=Springfield/O=Dis/CN=www.example.com" \
    %  -keyout ./priv/ssl/server.key -out ./priv/ssl/server.pem
    {ok, _} = cowboy:start_tls(rest_https, [
        {port, 8443},
        {certfile, os:getenv("REST_USERS_SSL_CERT", PrivDir ++ "/ssl/server.pem")},
        {keyfile, os:getenv("REST_USERS_SSL_KEY", PrivDir ++ "/ssl/server.key")}
    ], #{
        env => #{dispatch => Dispatch}
    }),
    rest_users_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(rest_https).

%% internal functions
