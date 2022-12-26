-module(db_q).

-export([add_user/3]).
-export([check_is_auth/1]).
-export([get_auth/2]).
-export([get_users/0]).
-export([update_user/4]).

-define(PGODECOPTS, #{decode_opts => [{return_rows_as_maps, true}, {column_name_as_atom, true}]}).

%  login!>,password->,name?> token<->,tokenexp<->,created->,encpass->,passsalt->,login->,name->
-spec add_user(bitstring(), bitstring(), bitstring()) -> map() | {error, any()}.
add_user(Name, Login, Password) ->
    Resp = pgo:query(Q =
        <<"SELECT login ",
          "FROM users ",
          "WHERE login = $1;">>,
        P = [Login],
        ?PGODECOPTS
    ),
    case Resp of
        #{num_rows := 0} ->
            PassSalt = helpers:new_passsalt(),
            EncPass = helpers:encode_password(Password, PassSalt),
            CreatedAt = os:system_time(seconds),
            Token = helpers:new_token(),
            TokenExp = helpers:new_token_expiration(CreatedAt),
            Resp1 = pgo:query(Q1 =
                <<"INSERT INTO users (name, login, password, passsalt, created_at, token, token_expiration) ",
                  "VALUES ($1, $2, $3, $4, $5, $6, $7) ",
                  "RETURNING token, token_expiration;">>,
                P1 = [Name, Login, EncPass, PassSalt, CreatedAt, Token, TokenExp],
                ?PGODECOPTS
            ),
            case Resp1 of
                #{num_rows := 1, rows := [Res]} ->
                    Res#{token_type => <<"Bearer">>};
                _ ->
                    {error, [Q1, P1, Resp1]}
            end;
        #{num_rows := 1} ->
            #{message => <<"login \"", Login/binary, "\" declined">>};
        _ ->
            {error, [Q, P, Resp]}
    end.

%  token!> tokenexp<?
-spec check_is_auth(bitstring()) -> boolean() | {error, any()}.
check_is_auth(Token) ->
    Resp = pgo:query(Q =
        <<"SELECT token_expiration ",
          "FROM users ",
          "WHERE token = $1;">>,
        P = [Token],
        ?PGODECOPTS
    ),
    case Resp of
        #{num_rows := 1, rows := [#{token_expiration := TokenExpiration}]} ->
            TokenExpiration > os:system_time(seconds);
        #{num_rows := 0} ->
            false;
        _ ->
            {error, [Q, P, Resp]}
    end.

%  login!>,password!> token<->,tokenexp<->,token_type<-
-spec get_auth(bitstring(), bitstring()) -> map() | {error, any()}.
get_auth(Login, Password) ->
    % Postgres will drop idle transaction session after <Timeout>
    Timeout = 10000,
    %TODO log(Error)
    pgo:query(<<"SET 'idle_in_transaction_session_timeout' = $1;">>, [Timeout]),
    pgo:transaction(fun() ->
        handle_auth(Login, Password)
    end).

%  -> [id,name]<-
-spec get_users() -> map() | {error, any()}.
get_users() ->
    Resp = pgo:query(Q =
        <<"SELECT id, name FROM users;">>,
        P = [],
        ?PGODECOPTS
    ),
    case Resp of
        #{num_rows := Num, rows := Res} ->
            #{count => Num, users => Res};
        _ ->
            {error, [Q, P, Resp]}
    end.

%  token!>,login!>,password!>,newpassword-> token<->,tokenexp<->,updated->,encpass->,passsault->
-spec update_user(bitstring(), bitstring(), bitstring(), bitstring()) -> map().
update_user(Token, Login, Password, NewPassword) ->
    % Postgres will drop idle transaction session after <Timeout>
    Timeout = 10000,
    %TODO log(Error)
    pgo:query(<<"SET 'idle_in_transaction_session_timeout' = $1;">>, [Timeout]),
    pgo:transaction(fun() ->
        handle_update_user(Token, Login, Password, NewPassword)
    end).

%%%% Private -------------------------------------------------------------------------------------
-spec handle_auth(bitstring(), 'ok' | tuple()) -> map() | {error, any()}.
handle_auth(Login, Password) ->
    Resp = pgo:query(Q =
        <<"SELECT password, passsalt ",
          "FROM users ",
          "WHERE login = $1 ",
          "FOR UPDATE NOWAIT;">>,
        P = [Login],
        ?PGODECOPTS
    ),
    case Resp of
        #{num_rows := 1, rows := [#{password := StoredPassword, passsalt := StoredPassSalt}]} ->
            % login exists
            IsPassword = helpers:encode_password(Password, StoredPassSalt) == StoredPassword,
            case IsPassword of
                true -> 
                    Token = helpers:new_token(),
                    TokenExpiration = helpers:new_token_expiration('new'),
                    Resp1 =
                        pgo:query(Q1 =
                            <<"UPDATE users ",
                            "SET token = $1, token_expiration = $2 ",
                            "WHERE login = $3 ",
                            "RETURNING token, token_expiration;">>,
                            P1 = [Token, TokenExpiration, Login],
                            ?PGODECOPTS
                        ),
                    case Resp1 of
                        #{num_rows := 1, rows := [Res]} ->
                            Res#{token_type => <<"Bearer">>};
                        _ ->
                            {error, [Q1, P1, Resp1]}
                    end;
                false ->
                    % wrong password
                    #{message => <<"incorrect login or password">>}
            end;
        #{num_rows := 0} ->
            % login missed
            #{message => <<"incorrect login or password">>};
        _ ->
            {error, [Q, P, Resp]}
    end.

-spec handle_update_user(bitstring(), bitstring(), bitstring(), bitstring()) -> map() | {error, any()}.
handle_update_user(Token, Login, Password, NewPassword) ->
    Resp = pgo:query(Q =
        <<"SELECT password, passsalt ",
          "FROM users ",
          "WHERE (token = $1 AND login = $2) ",
          "FOR UPDATE NOWAIT;">>,
        P = [Token, Login],
        ?PGODECOPTS
    ),
    case Resp of
        #{num_rows := 1, rows := [#{password := StoredPassword, passsalt := StoredPassSalt}]} ->
            IsPassword = helpers:encode_password(Password, StoredPassSalt) == StoredPassword,
            case IsPassword of
                true ->
                    PassSalt = helpers:new_passsalt(),
                    EncPass = helpers:encode_password(NewPassword, PassSalt),
                    UpdatedAt = os:system_time(seconds),
                    NewToken = helpers:new_token(),
                    NewTokenExp = helpers:new_token_expiration(UpdatedAt),
                    Resp1 =
                        pgo:query(Q1 =
                            <<"UPDATE users ",
                              "SET password = $1, passsalt = $2, token = $3, token_expiration = $4, updated_at = $5 ",
                              "WHERE token = $6 ",
                              "RETURNING token, token_expiration;">>,
                            P1 = [EncPass, PassSalt, NewToken, NewTokenExp, UpdatedAt, Token],
                            ?PGODECOPTS
                        ),
                    case Resp1 of
                        #{num_rows := 1, rows := [Res]} ->
                            Res#{token_type => <<"Bearer">>};
                        _Error1 ->
                            {error, [Q1, P1, Resp1]}
                    end;
                false ->
                    % wrong password
                    #{message => <<"incorrect login or password">>}
            end;
        #{num_rows := 0} ->
            #{message => <<"incorrect token or login">>};
        _ ->
            {error, [Q, P, Resp]}
    end.
