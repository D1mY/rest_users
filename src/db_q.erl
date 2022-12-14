-module(db_q).

-export([get_auth/2]).
-export([get_auth/1]).
-export([add_user/1]).
-export([update_user/1]).
-export([get_users/0]).

-define(PGODECOPTS, #{decode_opts => [{return_rows_as_maps, true}, {column_name_as_atom, true}]}).

get_auth(Login, Password) ->
    Login,
    Password,
    ok.

get_auth(Token) ->
    Token,
    ok.

add_user(UserObj) ->
    UserObj,
    ok.

update_user(UserObj) ->
    {"select * from", []},
    UserObj,
    ok.

get_users() ->
  pgo:query("SELECT id, name FROM users", ?PGODECOPTS).

% db_connect_and_query(Sql) ->
%   pgo:start_pool(default, #{pool_size => 5,
%                              host => "127.0.0.1",
%                              database => "usersdb",
%                              user => "restadmin",
%                              password => "r35t@dm1n",
%                              decode_opts => [{return_rows_as_maps, true}, {column_name_as_atom, true}]}),
%   pgo:query(Sql).

