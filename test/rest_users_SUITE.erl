-module(rest_users_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [
  {group, alice}
  % ,joe_list
  % ,{group, joe}
  % ,{group, joe_create}
  % ,{group, joe_login}
  % ,{group, joe_update}
].

groups() -> [
  {alice, [sequence], [users_post, users_get, auth_post, users_get, users_put, users_get]}
  % ,{joe, [sequence], [users_post, users_get, auth_post, users_get, users_put, users_get]}
  % ,{joe_create, [sequence], [users_post, users_get]}
  % ,{joe_login, [sequence], [auth_post, users_get]}
  % ,{joe_update, [sequence], [users_put, users_get]}
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

init_per_group(alice, Config) ->
  {ok, Body} = file:read_file(?config(data_dir, Config) ++ "/alice.json"),
  [{body, Body} | Config];
init_per_group(_,Config) ->
  Config.

end_per_group(_,_Config) -> ok.

% init_per_testcase(T, Config) when T =:= users_post ;
%                                   T =:= auth_post ;
%                                   T =:= users_put ->
%   {ok, _} = application:ensure_all_started(rest_users),
%   {ok, [gun]} = application:ensure_all_started(gun),
%   {ok, ConnPid} = gun:open("127.0.0.1", 8443, #{transport => tls}),
%   {ok, Body} = file:read_file(?config(data_dir, Config) ++
%                               "/" ++
%                               erlang:atom_to_list(T) ++
%                               "_body.json"),
%   [ {body, Body},
%     {gun_conn, ConnPid},
%     {content_type, {<<"content-type">>, <<"application/json">>}}
%     | Config];
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
  {save_config, ConfigList}.

%%% List users
users_get(Config) ->
  {_, SavedConfig} = ?config(saved_config, Config),
  Token = ?config(token, SavedConfig),
  true = erlang:is_bitstring(Token),
  ContentType = ?config(content_type, Config),
  ConnPid =     ?config(gun_conn, Config),
  AuthHeader = {<<"authorization">>, <<"Bearer ", Token/bitstring>>},

  StreamRef = gun:get(ConnPid, "/users", [ContentType, AuthHeader]),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {ok,
  #{<<"count">> := 2,
    <<"users">> := [
      #{<<"id">> := UserId1, <<"name">> := UserName1},
      #{<<"id">> := UserId2, <<"name">> := UserName2}
    ]}} = thoas:decode(ResBody),
  {save_config, SavedConfig}.

%%% Info auth
% auth_options(Config) -> ok.

%%% Info users
% users_options(Config) -> ok.

%%%% Fail ----------------------------------------------------------------------------------------


%%%% Drafts --------------------------------------------------------------------------------------
test_auth_post(Config) ->
  ContentType = ?config(content_type, Config),
  ReqBody = ?config(body, Config),
  ConnPid = ?config(gun_conn, Config),

  StreamRef = gun:post(ConnPid, "/auth", [ContentType], ReqBody),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, ResBody} = gun:await_body(ConnPid, StreamRef),

  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  {<<"cache-control">>, <<"no-store">>} = lists:keyfind(<<"cache-control">>, 1, Headers),
  {ok,
  #{<<"token">> := Token,
    <<"token_expiration">> := TokenExpiration,
    <<"token_type">> := <<"Bearer">>}} = thoas:decode(ResBody),
  {save_config,[{token, Token}, {token_expiration, TokenExpiration}]}.
