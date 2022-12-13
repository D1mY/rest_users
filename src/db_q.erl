-module(db_q).

-export([get_auth/2]).
-export([get_auth/1]).
-export([add_user/1]).
-export([update_user/1]).
-export([get_users/0]).

get_auth(Login, Password) ->
  Login, Password,
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
  ok.