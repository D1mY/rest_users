-module(users_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([is_conflict/2]).

-export([users_json/2]).
-export([users_h/2]).

%%%% API -----------------------------------------------------------------------------------------
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    Result =
        case cowboy_req:method(Req) of
            <<"POST">> ->
                true;
            _ ->
                is_auth_h(Req)
        end,
    {Result, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"PUT">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, users_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, users_h}], Req, State}.

is_conflict(Req, State) ->
    Result = false,
    {Result, Req, State}.

%%%% Handlers ------------------------------------------------------------------------------------
users_json(Req, State) ->
    Resp = db_q:get_users(),
    {thoas:encode(Resp), Req, State}.

users_h(Req, State) ->
    Method = cowboy_req:method(Req),
    {_, Body, _} = cowboy_req:read_body(Req),
    UserObj = thoas:decode(Body),
    Resp = method_h(Method, UserObj, Req),
    Status =
        case Resp of
            [{error, _}] -> false;
            _ -> true
        end,
    Req1 = cowboy_req:set_resp_body(thoas:encode(Resp), Req),
    Req2 = cowboy_req:set_resp_header(<<"cache-control">>, "no-store", Req1),
    {Status, Req2, State}.

%%%% Private -------------------------------------------------------------------------------------
is_auth_h(Req) ->
    IsAuth =
        case cowboy_req:parse_header(<<"authorization">>, Req) of
            {bearer, Token} ->
                db_q:check_auth(Token);
            _ ->
                false
        end,
    case IsAuth of
        true ->
            true;
        _ ->
            {false, <<"Bearer realm=\"cowboy\"">>}
    end.

method_h(_, {error, _Error}, _) ->
    %TODO log(_Error)
    [{error, <<"incorrect input">>}];
method_h(<<"POST">>, {ok, UserObj}, _) ->
    db_q:add_user(UserObj);
method_h(<<"PUT">>, {ok, UserObj}, Req) ->
    {bearer, Token} = cowboy_req:parse_header(<<"authorization">>, Req),
    db_q:update_user(UserObj#{token => Token}).
