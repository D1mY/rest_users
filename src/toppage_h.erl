-module(toppage_h).

-export([init/2]).
% -export([content_types_provided/2]).
% -export([content_types_accepted/2]).
% -export([hello_to_json/2]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
        },
        <<"{\"name\":\"rest_users\",",
            "\"version\":\"0.1.0\",",
            "\"links\":[
                {\"href\":\"/users\",
                 \"rel\":\"create\",
                 \"method\":\"POST\"},
                {\"href\":\"/auth\",
                 \"rel\":\"login\",
                 \"method\":\"POST\"},
                {\"href\":\"/users\",
                 \"rel\":\"update\",
                 \"method\":\"PUT\"},
                {\"href\":\"/users\",
                 \"rel\":\"list\",
                 \"method\":\"GET\"}
                ]}\n">>,
        Req0),
    % {cowboy_rest, Req, Opts}.
    {ok, Req, Opts}.

% content_types_provided(Req, State) ->
%     {[
%         {<<"application/json">>, hello_to_json}
%     ], Req, State}.

% content_types_accepted(Req, State) ->
%     {[
%         {<<"application/json">>, hello_to_json}
%     ], Req, State}.

% hello_to_json(Req, State) ->
%     {<<"{\"rest\": \"Hello World!\"}">>, Req, State}.