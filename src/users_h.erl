-module(users_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([options/2]).

-export([users_json/2]).
-export([users_h/2]).

%%%% API -----------------------------------------------------------------------------------------
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {true, Req, State};
        <<"OPTIONS">> ->
            {true, Req, State};
        _ ->
            {Res, Req1} = is_auth_h(Req),
            {Res, Req1, State}
    end.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"POST">>, <<"PUT">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, users_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, users_h}], Req, State}.

options(Req, State) ->
    {ok, JSON} = file:read_file("priv/users_body.json"),
    Req1 = cowboy_req:set_resp_body(JSON, Req),
    {ok, Req1, State}.

%%%% Handlers ------------------------------------------------------------------------------------
users_json(Req, State) ->
    Resp = db_q:get_users(),
    case Resp of
            {error, Error} ->
                Req1 = helpers:five00(Req, Error),
                {stop, Req1, State};
            _ ->
                Req1 = cowboy_req:set_resp_body(thoas:encode(Resp), Req),
                {true, Req1, State}
        end.

users_h(Req, State) ->
    Method = cowboy_req:method(Req),
    {_, Body, _} = cowboy_req:read_body(Req),
    UserObj = thoas:decode(Body),
    Resp = method_h(Method, UserObj, Req),
        case Resp of
            {error, Error} ->
                Req1 = helpers:five00(Req, Error),
                {stop, Req1, State};
            _ ->
                Req1 = cowboy_req:set_resp_body(thoas:encode(Resp), Req),
                Req2 = cowboy_req:set_resp_header(<<"cache-control">>, "no-store", Req1),
                {true, Req2, State}
        end.

%%%% Private -------------------------------------------------------------------------------------
is_auth_h(Req) ->
    IsAuth =
        case cowboy_req:parse_header(<<"authorization">>, Req) of
            {bearer, Token} ->
                db_q:check_is_auth(Token);
            _ ->
                false
        end,
    case IsAuth of
        true ->
            {true, Req};
        {error, Error} ->
            Req1 = helpers:five00(Req, Error),
            {stop, Req1};
        _ ->
            {{false, <<"Bearer realm=\"cowboy\"">>}, Req}
    end.

method_h(_, {error, _Error}, _) ->
    #{message => <<"incorrect JSON">>};
method_h(<<"POST">>, {ok, UserObj}, _) ->
    case UserObj of
        #{<<"login">> := Login, <<"password">> := Password} ->
            Name = maps:get(<<"name">>, UserObj, <<"Incognito">>),
            db_q:add_user(Name, Login, Password);
        _ ->
            #{message => <<"incorrect request body format">>}
    end;
method_h(<<"PUT">>, {ok, UserObj}, Req) ->
    {bearer, Token} = cowboy_req:parse_header(<<"authorization">>, Req),
    case UserObj of
        #{<<"login">> := Login, <<"password">> := Password, <<"newpassword">> := NewPassword} ->
            db_q:update_user(Token, Login, Password, NewPassword);
        _->
            #{message => <<"incorrect request body format">>}
    end.
