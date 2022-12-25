-module(rest_users_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [].

init_per_suite(Config) ->
  % {ok, [cowlib, gun]} = application:ensure_all_started(gun),
  % start pg, create users_test, add Joe A
  [{ct_hooks, [docker_compose_cth]} | Config].
end_per_suite(_Config) -> 
  application:stop(gun),
  application:stop(cowlib),
  ok.

init_per_testcase(test_post, Config) ->
  {ok, ConnPid} = gun:open("127.0.0.1", 8443, #{transport => tls}),
  {ok, Body} = file:read_file(?config(data_dir, Config) ++ "/auth_POST_body.json"),
  [ {body_post, Body},
    {gun_conn, ConnPid},
    {content_type, {<<"content-type">>, "application/json"}}
    | Config
  ];
init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  ConnPid = ?config(gun_conn, Config),
  gun:close(ConnPid).

test_options(_Config) ->
  ok.

test_post(Config) ->
  ContentType = ?config(content_type, Config),
  Body = ?config(body_post, Config),
  ConnPid = ?config(gun_conn, Config),
  PreT = os:system_time(second) + (60 * 60 * 24 * 30),

  StreamRef = gun:post(ConnPid, "/auth", [ContentType],Body),
  {response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef),
  {ok, Body} = gun:await_body(ConnPid, StreamRef),

  TAfter = os:system_time(second) + (60 * 60 * 24 * 30),

  {<<"cache-control">>, "no-store"} = lists:keyfind(<<"cache-control">>, 1, Headers),
  ContentType = lists:keyfind(<<"content-type">>, 1, Headers),
  #{<<"token">> := Token,
    <<"token_expiration">> := TokenExpiration,
    <<"token_type">> := <<"Bearer">>} = thoas:thoas_decode(Body),
  true = erlang:is_bitstring(Token),
  true = TokenExpiration >= PreT,
  true = TokenExpiration =< TAfter.

test_repost(Config) ->
  ContentType = ?config(content_type, Config),
  Body = ?config(body_post, Config),
  ConnPid = ?config(gun_conn, Config),
  StreamRef = gun:post(ConnPid, "/auth", [ContentType], Body),
  {response, nofin, 200, _Headers} = gun:await(ConnPid, StreamRef),
  {ok, Body} = gun:await_body(ConnPid, StreamRef).

test_put(_Config) -> ok.

test_reput(_Config) ->
  % mb include in test_put/1?
  ok.

test_puts(_Config) ->
  % do parallel PUT
  ok.

test_get(_Config) -> ok.

test_not_auth(_Config) -> ok.