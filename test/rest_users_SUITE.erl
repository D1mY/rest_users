-module(rest_users_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [
  {group, new_user}
  ,{group, options}
  ,{group, pg_row_lock}
].

groups() -> [
  {new_user, [sequence], [users_post, users_get, auth_post, users_get, users_put, users_get]}
  ,{options, [parallel], [auth_options, users_options]}
  ,{pg_row_lock, [parallel], [pg_locker, users_post_fail, auth_post_fail, users_put_fail, users_get]}
].

suite() ->
  [{require, name},
   {require, login},
   {require, password},
   {require, passsalt},
   {require, created_at},
   {require, token},
   {require, token_expiration}].

init_per_suite(Config) ->
  % [{ct_hooks, [docker_compose_cth]} | Config],
  %% start pg, create users_test, add Joe A
  %%  ct:get_config(name),
  %%  ct:get_config(login),
  %%  ct:get_config(password),
  %%  ct:get_config(passsalt),
  %%  ct:get_config(created_at),
  %%  ct:get_config(token),
  %%  ct:get_config(token_expiration),
   Config.
end_per_suite(_Config) -> 
  application:stop(gun),
  application:stop(cowlib),
  ok.

init_per_group(new_user, Config) ->
  {ok, Body} = file:read_file(?config(data_dir, Config) ++ "/new_user.json"),
  {ok, #{<<"name">> := ExpectedName}} = thoas:decode(Body),
  ExpectedStatus = 200,
  [{body, Body},
   {expected_name, ExpectedName},
   {expected_status, ExpectedStatus}
   | Config];
init_per_group(options, Config) ->
  PrivPath = code:priv_dir(rest_users),
  {ok, AuthJSON} =  file:read_file(PrivPath ++ "/auth_body.json"),
  {ok, UsersJSON} = file:read_file(PrivPath ++ "/users_body.json"),
  {ok, Auth} = thoas:decode(AuthJSON),
  {ok, Users} = thoas:decode(UsersJSON),
  ExpectedStatus = 200,
  [{auth_options, Auth},
   {users_options, Users},
   {expected_status, ExpectedStatus}
   | Config];
init_per_group(pg_row_lock, Config) ->
  UName =         ct:get_config(name),
  ULogin =        ct:get_config(login),
  UPassword =     <<"hello">>,
  UNewPassword =  <<"goodbye">>,
  Body = thoas:encode(#{name => UName,
                        login => ULogin,
                        password => UPassword,
                        newpassword => UNewPassword}),
  ExpectedStatus = 500,
  {ok, Body2} = file:read_file(?config(data_dir, Config) ++ "/new_user.json"),
  {ok, #{<<"name">> := ExpectedName}} = thoas:decode(Body2),
  [{body, Body},
   {expected_name, ExpectedName},
   {expected_status, ExpectedStatus}
   | Config];
init_per_group(_,Config) ->
  Config.

end_per_group(_,_Config) ->
  ok.

init_per_testcase(_, Config) ->
  {ok, _} = application:ensure_all_started(rest_users),
  {ok, _} = application:ensure_all_started(gun),
  {ok, ConnPid} = gun:open("127.0.0.1", 8443, #{transport => tls}),
  [ {gun_conn, ConnPid},
    {content_type, {<<"content-type">>, <<"application/json">>}}
    | Config].

end_per_testcase(_, Config) ->
  ConnPid = ?config(gun_conn, Config),
  gun:close(ConnPid).

%%%% Success -------------------------------------------------------------------------------------
%%% Create user (yields: token, token_expiration)
users_post(Config) ->
  ContentType = ?config(content_type, Config),
  ReqBody =     ?config(body, Config),
  ConnPid =     ?config(gun_conn, Config),

  StreamRef = gun:post(ConnPid, "/users", [ContentType], ReqBody),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {<<"cache-control">>, <<"no-store">>} = lists:keyfind(<<"cache-control">>, 1, Headers),
  {ok,
  #{<<"token">> := Token,
    <<"token_expiration">> := TokenExpiration,
    <<"token_type">> := <<"Bearer">>}} = thoas:decode(ResBody),
  true = TokenExpiration =< os:system_time(seconds) + (60 * 60 * 24 * 30),

  ConfigList = [{token, Token}, {token_expiration, TokenExpiration}],
  ct:comment("<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {save_config, ConfigList}.

%%% Login user (yields: token, token_expiration)
auth_post(Config) ->
  ContentType = ?config(content_type, Config),
  ReqBody =     ?config(body, Config),
  ConnPid =     ?config(gun_conn, Config),

  StreamRef = gun:post(ConnPid, "/auth", [ContentType], ReqBody),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {<<"cache-control">>, <<"no-store">>} = lists:keyfind(<<"cache-control">>, 1, Headers),
  {ok,
  #{<<"token">> := Token,
    <<"token_expiration">> := TokenExpiration,
    <<"token_type">> := <<"Bearer">>}} = thoas:decode(ResBody),
  true = TokenExpiration =< os:system_time(seconds) + (60 * 60 * 24 * 30),

  ConfigList = [{token, Token}, {token_expiration, TokenExpiration}],
  ct:comment("<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {save_config, ConfigList}.

%%% Update user (yields: token, token_expiration)
users_put(Config) ->
  ContentType = ?config(content_type, Config),
  ReqBody =     ?config(body, Config),
  ConnPid =     ?config(gun_conn, Config),
  {_, SavedConfig} = ?config(saved_config, Config),
  ReqToken =    ?config(token, SavedConfig),
  AuthHeader = {<<"authorization">>, <<"Bearer ", ReqToken/binary>>},

  StreamRef = gun:put(ConnPid, "/users", [ContentType, AuthHeader], ReqBody),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {<<"cache-control">>, <<"no-store">>} = lists:keyfind(<<"cache-control">>, 1, Headers),
  {ok,
  #{<<"token">> := Token,
    <<"token_expiration">> := TokenExpiration,
    <<"token_type">> := <<"Bearer">>}} = thoas:decode(ResBody),
  true = TokenExpiration =< os:system_time(seconds) + (60 * 60 * 24 * 30),

  ConfigList = [{token, Token}, {token_expiration, TokenExpiration}],
  ct:comment("<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {save_config, ConfigList}.

%%% List users
users_get(Config) ->
  SavedConfig = case ?config(saved_config, Config) of
    {_, SC} ->
      SC;
    undefined ->
      [{token, ct:get_config(token)}]
  end,
  Token = ?config(token, SavedConfig),
  true = erlang:is_bitstring(Token),
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  AuthHeader = {<<"authorization">>, <<"Bearer ", Token/bitstring>>},
  Username1 = ct:get_config(name),
  UserName2 = ?config(expected_name, Config),

  StreamRef = gun:get(ConnPid, "/users", [ContentType, AuthHeader]),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {ok,
  #{<<"count">> := 2,
    <<"users">> := [
      #{<<"id">> := 1, <<"name">> := Username1},
      #{<<"id">> := 2, <<"name">> := UserName2}
    ]}} = thoas:decode(ResBody),
  ct:comment("<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {save_config, SavedConfig}.

%%% Info auth
auth_options(Config) -> 
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  Options = ?config(auth_options, Config),

  StreamRef = gun:options(ConnPid, "/auth", [ContentType]),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  ct:comment("<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {ok, Options} = thoas:decode(ResBody).

%%% Info users
users_options(Config) -> 
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  Options = ?config(users_options, Config),

  StreamRef = gun:options(ConnPid, "/users", [ContentType]),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  ct:comment("<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {ok, Options} = thoas:decode(ResBody).

%%%% Fail ----------------------------------------------------------------------------------------
%%% cases: duplicate login = 200 | pg row lock = 500 | bad request = 400
%%% Create user fail 
users_post_fail(Config) ->
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  ReqBody =     ?config(body, Config),
  StatusCode =  400, % user exists

  StreamRef = gun:post(ConnPid, "/users", [ContentType], ReqBody),
  {response, nofin, StatusCode, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {ok,
  #{<<"message">> := <<"login \"joe\" declined">>}} = thoas:decode(ResBody),
  {comment, "<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"}.

%%% Login user fail
auth_post_fail(Config) ->
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  ReqBody =     ?config(body, Config),
  StatusCode =  ?config(expected_status, Config),

  StreamRef = gun:post(ConnPid, "/auth", [ContentType], ReqBody),
  {response, nofin, StatusCode, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {ok,
  #{<<"message">> := <<"oops! try again">>}} = thoas:decode(ResBody),
  {comment, "<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"}.

%%% Update user fail
users_put_fail(Config) ->
  ContentType = ?config(content_type, Config),
  ReqBody =     ?config(body, Config),
  ConnPid =     ?config(gun_conn, Config),
  ReqToken =    ct:get_config(token),
  AuthHeader = {<<"authorization">>, <<"Bearer ", ReqToken/bitstring>>},
  StatusCode =  ?config(expected_status, Config),

  StreamRef = gun:put(ConnPid, "/users", [ContentType, AuthHeader], ReqBody),
  {response, nofin, StatusCode, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {ok,
  #{<<"message">> := <<"oops! try again">>}} = thoas:decode(ResBody),
  {comment, "<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"}.

%%% List users should never fail

%%% PG row level lock transaction
pg_locker(_Config) ->
  Res = 
  pgo:transaction(fun()->
  Resp = 
    pgo:query(
        <<"SELECT name, token ",
          "FROM users ",
          "WHERE (login = $1) ",
          "FOR UPDATE NOWAIT;">>,
        [ct:get_config(login)]
    ),
    timer:sleep(1234),
    Resp
    end),
    {comment, Res}.

%%%% Drafts --------------------------------------------------------------------------------------
