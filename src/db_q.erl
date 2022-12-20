-module(db_q).

-export([add_user/1]).
-export([check_auth/1]).
-export([get_auth/2]).
-export([get_users/0]).
-export([update_user/1]).

-define(PGODECOPTS, #{decode_opts => [{return_rows_as_maps, true}, {column_name_as_atom, true}]}).

%  login->,password->,name?> token<->,tokenexp<->,created->,encpass->,passsalt->,login->,name->
-spec add_user(map()) -> list().
add_user(#{<<"login">> := Login, <<"password">> := Password} = UserObj) ->
    Name = maps:get(<<"name">>, UserObj, <<"Incognito">>),
    case check_new_l(Login) of
        true ->
            PassSalt = helpers:new_passsalt(),
            EncPass = helpers:encode_password(Password, PassSalt),
            CreatedAt = os:system_time(seconds),
            Token = helpers:new_token(),
            TokenExp = helpers:new_token_expiration(CreatedAt),
            Resp = pgo:query(
                <<"INSERT INTO users (name, login, password, passsalt, created_at, token, token_expiration) VALUES ($1, $2, $3, $4, $5, $6, $7) RETURNING token, token_expiration;">>,
                [Name, Login, EncPass, PassSalt, CreatedAt, Token, TokenExp],
                ?PGODECOPTS
            ),
            case Resp of
                #{num_rows := 1, rows := [Res]} ->
                    [Res#{token_type => <<"Bearer">>}];
                _ ->
                    [{error, <<"oops! try again">>}]
            end;
        false ->
            [{error, <<"login \"", Login/binary, "\" declined">>}]
    end;
add_user(_Any) ->
    [{error, <<"incorrect format">>}].

%  token-> ?tokenexp<-
-spec check_auth(bitstring()) -> boolean().
check_auth(Token) ->
    Res = pgo:query(
        <<"SELECT token_expiration FROM users WHERE token = $1;">>, [Token], ?PGODECOPTS
    ),
    case Res of
        #{num_rows := 1, rows := [#{token_expiration := TokenExpiration}]} ->
            TokenExpiration > os:system_time(seconds);
        _ ->
            false
    end.

%  login->,password-> token<->,tokenexp<->,token_type<-
-spec get_auth(bitstring(), bitstring()) -> list().
get_auth(Login, Password) ->
    % Postgres will drop idle transaction session after <Timeout>
    Timeout = 10000,
    pgo:query(<<"SET 'idle_in_transaction_session_timeout' = $1;">>, [Timeout]),
    pgo:transaction(fun() ->
        handle_auth(Login, Password)
    end).

%  -> [id,name]<-
-spec get_users() -> list().
get_users() ->
    case pgo:query(<<"SELECT id, name FROM users;">>, [], ?PGODECOPTS) of
        #{num_rows := Num, rows := Resp} ->
            [#{count => Num, users => Resp}];
        _ ->
            [{error, <<"unavailable">>}]
    end.

%  token->,login->,password->,newpassword-> token<->,tokenexp<->,updated->,encpass->,passsault->
update_user(#{
    token := Token,
    <<"login">> := Login,
    <<"password">> := Password,
    <<"newpassword">> := NewPassword
}) ->
    % Postgres will drop idle transaction session after <Timeout>
    Timeout = 10000,
    pgo:query(<<"SET 'idle_in_transaction_session_timeout' = $1;">>, [Timeout]),
    pgo:transaction(fun() ->
        handle_update_user(Token, Login, Password, NewPassword)
    end);
update_user(_Any) ->
    [{error, <<"incorrect format">>}].

%%%% Private -------------------------------------------------------------------------------------
-spec check_l(bitstring()) -> tuple().
check_l(Login) ->
    Resp =
        pgo:query(
            <<"SELECT password, passsalt FROM users WHERE login = $1 FOR UPDATE NOWAIT;">>,
            [Login],
            ?PGODECOPTS
        ),
    case Resp of
        #{num_rows := 1, rows := [#{password := StoredPassword, passsalt := StoredPassSalt}]} ->
            % login exists
            {'ok', {StoredPassword, StoredPassSalt}};
        _ ->
            % login missed
            {false, <<"incorrect login">>}
    end.

check_new_l(Login) ->
    case pgo:query(<<"SELECT login FROM users WHERE login = $1;">>, [Login], ?PGODECOPTS) of
        #{num_rows := 1, rows := [#{login := Login}]} ->
            false;
        _ ->
            true
    end.

-spec check_p(bitstring() | 'ok', tuple()) -> 'ok' | tuple().
check_p(Password, {'ok', {StoredPassword, StoredPassSalt}}) ->
    Res =
        % password match
        helpers:encode_password(Password, StoredPassSalt) == StoredPassword,
    check_p('ok', {Res, <<"incorrect password">>});
check_p(_, {false, _Error}) ->
    %TODO log(_Error),

    % hide details
    {error, <<"incorrect login or password">>};
check_p(_, {true, _}) ->
    'ok'.

-spec handle_auth(bitstring(), 'ok' | tuple()) -> list().
handle_auth(Login, 'ok') ->
    Token = helpers:new_token(),
    TokenExpiration = helpers:new_token_expiration('new'),
    Resp =
        pgo:query(
            <<"UPDATE users SET token = $1, token_expiration = $2 WHERE login = $3 RETURNING token, token_expiration;">>,
            [Token, TokenExpiration, Login],
            ?PGODECOPTS
        ),
    case Resp of
        #{num_rows := 1, rows := [Res]} ->
            [Res#{token_type => <<"Bearer">>}];
        _ ->
            [{error, <<"oops! try again">>}]
    end;
handle_auth(_, {error, Error}) ->
    [{error, Error}];
handle_auth(Login, Password) ->
    handle_auth(Login, check_p(Password, check_l(Login))).

handle_update_user(Token, Login, Password, NewPassword) ->
    Resp0 = pgo:query(
        <<"SELECT password, passsalt FROM users WHERE (token = $1 AND login = $2) FOR UPDATE NOWAIT;">>,
        [Token, Login],
        ?PGODECOPTS
    ),
    case Resp0 of
        #{num_rows := 1, rows := [#{password := StoredPassword, passsalt := StoredPassSalt}]} ->
            case check_p(Password, {'ok', {StoredPassword, StoredPassSalt}}) of
                'ok' ->
                    PassSalt = helpers:new_passsalt(),
                    EncPass = helpers:encode_password(NewPassword, PassSalt),
                    UpdatedAt = os:system_time(seconds),
                    NewToken = helpers:new_token(),
                    NewTokenExp = helpers:new_token_expiration(UpdatedAt),
                    Resp =
                        pgo:query(
                            <<"UPDATE users SET password = $1, passsalt = $2, token = $3, token_expiration = $4, updated_at = $5 WHERE token = $6 RETURNING token, token_expiration;">>,
                            [EncPass, PassSalt, NewToken, NewTokenExp, UpdatedAt, Token],
                            ?PGODECOPTS
                        ),
                    case Resp of
                        #{num_rows := 1, rows := [Res]} ->
                            [Res#{token_type => <<"Bearer">>}];
                        _ ->
                            [{error, <<"oops! try again">>}]
                    end;
                Error ->
                    [Error]
            end;
        _ ->
            [{error, <<"incorrect token or login">>}]
    end.
