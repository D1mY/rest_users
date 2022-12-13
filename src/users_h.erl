-module(users_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([is_conflict/2]).
-export([users_json/2]).
-export([users_h/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
  case cowboy_req:method(Req) of
    <<"POST">> ->
      {true, Req, State};
    _ ->
      case cowboy_req:parse_header(<<"authorization">>, Req) of
        {bearer, Token} ->
          Result = db_q:get_auth(Token),
          {Result, Req, State};
        _ ->
          {{false, <<"Bearer realm=\"cowboy\"">>}, Req, State}
      end
  end.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"PUT">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, users_json}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {
    [
      {<<"application/json">>, users_h}
    ], Req, State}.

is_conflict(Req, State) ->
  Result = false,
  {Result, Req, State}.

users_json(Req, State) ->
  Resp = db_q:get_users(),
  Req1 = cowboy_req:set_resp_body(thoas:encode(Resp), Req),
  {true, Req1, State}.

users_h(Req, State) ->
  Method = cowboy_req:method(Req),
  req_h(Method),
  {true, Req, State}.

req_h(<<"POST">>) ->
  ok;
req_h(<<"PUT">>) ->
  ok.
