-module(toppage_h).

-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req0),
    PrivPath = code:priv_dir(rest_users),
    {ok, JSON} = file:read_file(PrivPath ++ "/toppage_body.json"),
    Req1 = helpers:two00(Req, JSON),
    {ok, Req1, Opts}.
