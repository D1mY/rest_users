-module(rest_users_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [
  {group, new_user}
  ,{group, options}
  ,{group, pg_row_lock}
  ,{group, new_incognito_user}
  ,{group, incorrect_token}
  ,{group, incorrect_login}
  ,{group, incorrect_password}
  ,{group, incorrect_req_body}
].

groups() -> [
  {new_user, [sequence], [users_post, users_get, auth_post, users_get, users_put, users_get]}
  ,{new_incognito_user, [sequence], [users_post, users_get]}
  ,{options, [parallel], [auth_options, users_options]}
  ,{pg_row_lock, [parallel], [pg_locker, users_post_fail, auth_post_fail, users_put_fail, users_get]}
  ,{incorrect_token, [sequence], [users_put_fail, users_get]}
  ,{incorrect_login, [sequence], [auth_post_fail, users_put_fail]}
  ,{incorrect_password, [sequence], [auth_post_fail, users_put_fail]}
  ,{incorrect_req_body, [sequence], [users_post_fail, auth_post_fail, users_put_fail]}
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
  %% start pgo, clear users, add Joe A
  {ok, _} = application:ensure_all_started(rest_users),
  pgo:query("truncate users restart identity;"),
  pgo:query("insert into users 
    (name, login, password, passsalt, created_at, token, token_expiration)
    values ($1, $2, $3, $4, $5, $6, $7);", [
    ct:get_config(name),
    ct:get_config(login),
    ct:get_config(password),
    ct:get_config(passsalt),
    ct:get_config(created_at),
    ct:get_config(token),
    ct:get_config(token_expiration)]
  ),
  Config.
end_per_suite(_Config) -> 
  application:stop(gun),
  application:stop(cowlib),
  ok.

init_per_group(new_user, Config) ->
  {ok, Body} = file:read_file(?config(data_dir, Config) ++ "/new_user.json"),
  {ok, #{<<"name">> := ExpectedName}} = thoas:decode(Body),
  UsersGetExpectedResp = #{<<"count">> => 2,
      <<"users">> => [
      #{<<"id">> => 1, <<"name">> => ct:get_config(name)},
      #{<<"id">> => 2, <<"name">> => ExpectedName}
    ]},
  ct:comment("Request body: <pre>" ++ erlang:binary_to_list(Body) ++ "</pre>"),
  [{body, Body},
   {users_get_expected_status, 200},
   {users_get_expected_resp, UsersGetExpectedResp}
   | Config];
init_per_group(new_incognito_user, Config) ->
  init_per_suite(Config),
  Body = <<"{\"login\":\"john\", \"password\":\"doe\"}">>,
  UsersGetExpectedResp = #{<<"count">> => 2,
      <<"users">> => [
      #{<<"id">> => 1, <<"name">> => ct:get_config(name)},
      #{<<"id">> => 2, <<"name">> => <<"Incognito">>}
    ]},
  ct:comment("Request body: <pre>" ++ erlang:binary_to_list(Body) ++ "</pre>"),
  [{body, Body},
   {users_get_expected_status, 200},
   {users_get_expected_resp, UsersGetExpectedResp}
   | Config];
init_per_group(options, Config) ->
  PrivPath = code:priv_dir(rest_users),
  {ok, AuthJSON} =  file:read_file(PrivPath ++ "/auth_body.json"),
  {ok, UsersJSON} = file:read_file(PrivPath ++ "/users_body.json"),
  {ok, Auth} = thoas:decode(AuthJSON),
  {ok, Users} = thoas:decode(UsersJSON),
  [{auth_options, Auth},
   {users_options, Users},
   {expected_status, 200}
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
  {ok, Body2} = file:read_file(?config(data_dir, Config) ++ "/new_user.json"),
  {ok, #{<<"name">> := ExpectedName}} = thoas:decode(Body2),
  UsersPostExpectedResp = #{<<"message">> => <<"login \"", ULogin/bitstring, "\" declined">>},
  UsersGetExpectedResp = #{<<"count">> => 2,
      <<"users">> => [
      #{<<"id">> => 1, <<"name">> => UName},
      #{<<"id">> => 2, <<"name">> => ExpectedName}
    ]},
  Five00ExpectedResp = #{<<"message">> => <<"oops! try again">>},
  ct:comment("Request body: <pre>" ++ erlang:binary_to_list(Body) ++ "</pre>"),
  [{body, Body},
   {users_post_expected_status, 400},
   {users_post_expected_resp, UsersPostExpectedResp},
   {auth_post_expected_status, 500},
   {auth_post_expected_resp, Five00ExpectedResp},
   {users_put_token, ct:get_config(token)},
   {users_put_expected_status, 500},
   {users_put_expected_resp, Five00ExpectedResp},
   {users_get_token, ct:get_config(token)},
   {users_get_expected_status, 200},
   {users_get_expected_resp, UsersGetExpectedResp}
   | Config];
init_per_group(incorrect_token, Config) ->
  Body = thoas:encode(#{login => ct:get_config(login),
                        password => <<"hello">>}),
  Token = <<"SomeIncorrectNonDBPresentedToken">>,
  Four01ExpectedResp = [],
  ct:comment("Token: <pre>" ++ erlang:binary_to_list(Token) ++ 
             "</pre>Request body: <pre>" ++ erlang:binary_to_list(Body) ++ "</pre>"),
  [{body, Body},
   {users_put_token, Token},
   {users_put_expected_status, 401},
   {users_put_expected_resp, Four01ExpectedResp},
   {users_get_token, Token},
   {users_get_expected_status, 401},
   {users_get_expected_resp, Four01ExpectedResp}
   | Config];
init_per_group(incorrect_login, Config) ->
  Body = thoas:encode(#{login => <<"mike">>,
                        password => <<"hello">>,
                        newpassword => <<"goodbye">>}),
  Token = ct:get_config(token),
  AuthPostExpectedResp = #{<<"message">> => <<"incorrect login or password">>},
  UsersPutExpectedResp = #{<<"message">> => <<"incorrect token or login">>},
  ct:comment("Request body: <pre>" ++ erlang:binary_to_list(Body) ++ "</pre>"),
  [{body, Body},
   {auth_post_expected_status, 200},
   {auth_post_expected_resp, AuthPostExpectedResp},
   {users_put_token, Token},
   {users_put_expected_status, 400},
   {users_put_expected_resp, UsersPutExpectedResp}
   | Config];
init_per_group(incorrect_password, Config) ->
  Body = thoas:encode(#{login => <<"joe">>,
                        password => <<"goodbye">>,
                        newpassword => <<"Kool-Aid">>}),
  Token = ct:get_config(token),
  ExpectedResp = #{<<"message">> => <<"incorrect login or password">>},
  ct:comment("Request body: <pre>" ++ erlang:binary_to_list(Body) ++ "</pre>"),
  [{body, Body},
   {auth_post_expected_status, 200},
   {auth_post_expected_resp, ExpectedResp},
   {users_put_token, Token},
   {users_put_expected_status, 400},
   {users_put_expected_resp, ExpectedResp}
   | Config];
init_per_group(incorrect_req_body, Config) ->
  Body = thoas:encode(#{body => <<"incorrect">>}),
  Token = ct:get_config(token),
  ExpectedResp = #{<<"message">> => <<"incorrect request body format">>},
  ct:comment("Request body: <pre>" ++ erlang:binary_to_list(Body) ++ "</pre>"),
  [{body, Body},
   {users_post_expected_status, 400},
   {users_post_expected_resp, ExpectedResp},
   {auth_post_expected_status, 400},
   {auth_post_expected_resp, ExpectedResp},
   {users_put_token, Token},
   {users_put_expected_status, 400},
   {users_put_expected_resp, ExpectedResp}
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
%% full request body
%% no name request body
users_post(Config) ->
  %% ready
  ContentType = ?config(content_type, Config),
  ReqBody =     ?config(body, Config),
  ConnPid =     ?config(gun_conn, Config),
  %% steady
  StreamRef = gun:post(ConnPid, "/users", [ContentType], ReqBody),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),
  %% go
  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {<<"cache-control">>, <<"no-store">>} = lists:keyfind(<<"cache-control">>, 1, Headers),
  {ok,
  #{<<"token">> := Token,
    <<"token_expiration">> := TokenExpiration,
    <<"token_type">> := <<"Bearer">>}} = thoas:decode(ResBody),
  true = TokenExpiration =< os:system_time(seconds) + (60 * 60 * 24 * 30),
  %% handover
  ConfigList = [{token, Token}, {token_expiration, TokenExpiration}],
  ct:comment("200:<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {save_config, ConfigList}.

%%% Login user (yields: token, token_expiration)
auth_post(Config) ->
  %% ready
  ContentType = ?config(content_type, Config),
  ReqBody =     ?config(body, Config),
  ConnPid =     ?config(gun_conn, Config),
  %% steady
  StreamRef = gun:post(ConnPid, "/auth", [ContentType], ReqBody),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),
  %% go
  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {<<"cache-control">>, <<"no-store">>} = lists:keyfind(<<"cache-control">>, 1, Headers),
  {ok,
  #{<<"token">> := Token,
    <<"token_expiration">> := TokenExpiration,
    <<"token_type">> := <<"Bearer">>}} = thoas:decode(ResBody),
  true = TokenExpiration =< os:system_time(seconds) + (60 * 60 * 24 * 30),
  %% handover
  ConfigList = [{token, Token}, {token_expiration, TokenExpiration}],
  ct:comment("200:<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {save_config, ConfigList}.

%%% Update user (yields: token, token_expiration)
users_put(Config) ->
  %% ready
  ContentType = ?config(content_type, Config),
  ReqBody =     ?config(body, Config),
  ConnPid =     ?config(gun_conn, Config),
  {_, SavedConfig} = ?config(saved_config, Config),
  ReqToken =    ?config(token, SavedConfig),
  AuthHeader =  {<<"authorization">>, <<"Bearer ", ReqToken/binary>>},
  %% steady
  StreamRef = gun:put(ConnPid, "/users", [ContentType, AuthHeader], ReqBody),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),
  %% go
  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {<<"cache-control">>, <<"no-store">>} = lists:keyfind(<<"cache-control">>, 1, Headers),
  {ok,
  #{<<"token">> := Token,
    <<"token_expiration">> := TokenExpiration,
    <<"token_type">> := <<"Bearer">>}} = thoas:decode(ResBody),
  true = TokenExpiration =< os:system_time(seconds) + (60 * 60 * 24 * 30),
  %% handover
  ConfigList = [{token, Token}, {token_expiration, TokenExpiration}],
  ct:comment("200:<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {save_config, ConfigList}.

%%% List users
users_get(Config) ->
  %% ready
  SavedConfig = case ?config(saved_config, Config) of
    {_, SC} ->
      SC;
    undefined ->
      [{token, ?config(users_get_token, Config)}]
  end,
  Token = ?config(token, SavedConfig),
  true = erlang:is_bitstring(Token),
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  AuthHeader =  {<<"authorization">>, <<"Bearer ", Token/bitstring>>},
  StatusCode =  ?config(users_get_expected_status, Config),
  ExpectedResp =  ?config(users_get_expected_resp, Config),
  %% steady
  StreamRef = gun:get(ConnPid, "/users", [ContentType, AuthHeader]),
  {response, IsFin, StatusCode, Headers} = gun:await(ConnPid, StreamRef),
  ResBody = ricochet(IsFin, ConnPid, StreamRef),
  %% go

  ContentType = headshot(ResBody, Headers, ContentType),
  {ok, ExpectedResp} = thoas:decode(ResBody),
  %% handover
  ct:comment(erlang:integer_to_list(StatusCode) ++ ":<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {save_config, SavedConfig}.

%%% Info auth
auth_options(Config) ->
  %% ready
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  Options =     ?config(auth_options, Config),
  %% steady
  StreamRef = gun:options(ConnPid, "/auth", [ContentType]),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),
  %% go
  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  ct:comment("200:<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {ok, Options} = thoas:decode(ResBody).

%%% Info users
users_options(Config) ->
  %% ready
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  Options =     ?config(users_options, Config),
  %% steady
  StreamRef = gun:options(ConnPid, "/users", [ContentType]),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),
  %% go
  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  ct:comment("200:<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"),
  {ok, Options} = thoas:decode(ResBody).

%%%% Fail ----------------------------------------------------------------------------------------
%%%% duplicate login = 400 | pg row lock = 500 | bad request = 400
%%% Create user fail 
%% Existing user
%% Incorrect request body
users_post_fail(Config) ->
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  ReqBody =     ?config(body, Config),
  StatusCode =  ?config(users_post_expected_status, Config),
  ExpectedResp =  ?config(users_post_expected_resp, Config),

  StreamRef = gun:post(ConnPid, "/users", [ContentType], ReqBody),
  {response, IsFin, StatusCode, Headers} = gun:await(ConnPid, StreamRef),
  ResBody = ricochet(IsFin, ConnPid, StreamRef),

  ContentType = headshot(ResBody, Headers, ContentType),
  {ok, ExpectedResp} = thoas:decode(ResBody),
  {comment, erlang:integer_to_list(StatusCode) ++ ":<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"}.

%%% Login user fail
%% PG row locked
%% Incorrect login
%% Incorrect password
auth_post_fail(Config) ->
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  ReqBody =     ?config(body, Config),
  StatusCode =  ?config(auth_post_expected_status, Config),
  ExpectedResp =  ?config(auth_post_expected_resp, Config),

  StreamRef = gun:post(ConnPid, "/auth", [ContentType], ReqBody),
  {response, IsFin, StatusCode, Headers} = gun:await(ConnPid, StreamRef),
  ResBody = ricochet(IsFin, ConnPid, StreamRef),

  ContentType = headshot(ResBody, Headers, ContentType),
  {ok,ExpectedResp} = thoas:decode(ResBody),
  {comment, erlang:integer_to_list(StatusCode) ++ ":<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"}.

%%% Update user fail
%% PG row locked
%% Incorrect token
%% Incorrect login
%% Incorrect password
users_put_fail(Config) ->
  ContentType = ?config(content_type, Config),
  ReqBody =     ?config(body, Config),
  ConnPid =     ?config(gun_conn, Config),
  ReqToken =    ?config(users_put_token, Config),
  % ReqToken =    ct:get_config(token),
  AuthHeader = {<<"authorization">>, <<"Bearer ", ReqToken/bitstring>>},
  StatusCode =  ?config(users_put_expected_status, Config),
  ExpectedResp =  ?config(users_put_expected_resp, Config),

  StreamRef = gun:put(ConnPid, "/users", [ContentType, AuthHeader], ReqBody),
  {response, IsFin, StatusCode, Headers} = gun:await(ConnPid, StreamRef),
  ResBody = ricochet(IsFin, ConnPid, StreamRef),

  ContentType = headshot(ResBody, Headers, ContentType),
  {ok,ExpectedResp} = thoas:decode(ResBody),
  {comment, erlang:integer_to_list(StatusCode) ++ ":<pre>" ++ erlang:binary_to_list(ResBody) ++ "</pre>"}.

%%% List users should never fail on PG row lock

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

%%% GUN response body
ricochet(IsFin, ConnPid, StreamRef) ->
  case IsFin of
    nofin ->
      {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
      RespBody;
    fin ->
      <<"[]">>
    end.
%%% GUN response headers
headshot(ResBody, Headers, ContentType) ->
  case ResBody of
    <<"[]">> ->
      ContentType;
    _ ->
      lists:keyfind(<<"content-type">>, 1, Headers)
    end.
