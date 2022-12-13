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
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    rest_users_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% internal functions
