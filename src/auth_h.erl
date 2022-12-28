-module(auth_h).

-export([init/2]).

-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([options/2]).

-export([auth_json/2]).

%%%% API -----------------------------------------------------------------------------------------
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, auth_json}], Req, State}.

options(Req, State) ->
    PrivPath = code:priv_dir(rest_users),
    {ok, JSON} = file:read_file(PrivPath ++ "/auth_body.json"),
    Req1 = cowboy_req:set_resp_body(JSON, Req),
    Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req1),
    {ok, Req2, State}.

%%%% Handlers ------------------------------------------------------------------------------------
auth_json(Req0, State) ->
    Req = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req0),
    {_, Body, _} = cowboy_req:read_body(Req),
    case thoas:decode(Body) of
        {ok, #{<<"login">> := Lo, <<"password">> := Pa}} ->
            case db_q:get_auth(Lo, Pa) of
                {error, Error} ->
                    Req1 = helpers:five00(Req, Error),
                    {stop, Req1, State};
                Resp ->
                    Req1 = cowboy_req:set_resp_body(thoas:encode(Resp), Req),
                    Req2 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-store">>, Req1),
                {true, Req2, State}
                end;
        _ ->
            {false, Req, State}
    end.
