-module(toppage_h).

-export([init/2]).
% -export([content_types_provided/2]).
% -export([content_types_accepted/2]).
% -export([hello_to_json/2]).

init(Req, Opts) ->
    {ok, JSON} = file:read_file("priv/toppage_body.json"),
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
%     {<<"{\"rest\": \"Hello from rest_users\"}">>, Req, State}.