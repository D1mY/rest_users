-module(toppage_h).

-export([init/2]).
% -export([content_types_provided/2]).
% -export([content_types_accepted/2]).
% -export([hello_to_json/2]).

init(Req0, Opts) ->
    Req = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req0),
    PrivPath = code:priv_dir(rest_users),
    {ok, JSON} = file:read_file(PrivPath ++ "/toppage_body.json"),
    Req1 = helpers:two00(Req, JSON),
    {ok, Req1, Opts}.
    % {cowboy_rest, Req1, Opts}.

% content_types_provided(Req, State) ->
%     {[
%         {<<"application/json">>, hello_to_json}
%     ], Req, State}.

% content_types_accepted(Req, State) ->
%     {[
%         {<<"application/json">>, hello_to_json}
%     ], Req, State}.

% hello_to_json(Req, State) ->
%     {ok, JSON} = file:read_file("priv/toppage_body.json"),
%     {JSON, Req, State}.