%%%-------------------------------------------------------------------
%% @doc rest_users top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rest_users_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 10},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
