-module(auth_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([auth_json/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, auth_json}
  ], Req, State}.

auth_json(Req, State) ->
  {_, Body, _} = cowboy_req:read_body(Req),
  case thoas:decode(Body) of
    {ok, #{<<"login">> := Lo, <<"password">> := Pa}} ->
      Resp = db_q:get_auth(Lo,Pa),
      Req1 = cowboy_req:set_resp_body(thoas:encode(Resp), Req),
      {true, Req1, State};
    _ ->
      {false, Req, State}
  end.
