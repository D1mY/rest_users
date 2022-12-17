-module(db_q).

-export([get_auth/2]).
-export([get_auth/1]).
-export([add_user/1]).
-export([update_user/1]).
-export([get_users/0]).

-define(PGODECOPTS, #{decode_opts => [{return_rows_as_maps, true}, {column_name_as_atom, true}]}).

%  login->,password-> token<->,tokenexp<->,token_type<-
-spec get_auth(bitstring(), bitstring()) -> list().
get_auth(Login, Password) ->
    pgo:transaction(fun() ->
        % Postgres will drop idle transaction session after <timeout>
        pgo:query("SET idle_in_transaction_session_timeout = $1;", [10000]), 
        handle_auth(Login, Password)
    end).

%  token-> ?tokenexp<-
-spec get_auth(bitstring()) -> boolean().
get_auth(Token) ->
    Res = pgo:query("SELECT token_expiration FROM users WHERE token = $1;", [Token], ?PGODECOPTS),
    case Res of
        #{num_rows := 1, rows := [#{token_expiration := TokenExpiration}]} ->
            TokenExpiration > os:system_time(seconds);
        _ ->
            false
    end.

%  login->,password->,name?> token<->,tokenexp<->,created->,encpass->,passsalt->,login->,name?>
add_user(UserObj) ->
    #{login := _Login, password := _Password} = UserObj,
    % "UPDATE users SET updated_at = current_timestamp(0)"
    ok.

%  login->,password->,newpassword-> token<->,tokenexp<->,updated->,encpass->,passsault->
update_user(UserObj) ->
    % #{login := Login, password := Password, newpassword := NewPassword} = UserObj,
    %   pgo:transaction(fun() ->
    %     % Postgres will drop idle transaction session after <timeout>
    %       pgo:query(<<"SET idle_in_transaction_session_timeout = $1;">>, [10000], ?PGODECOPTS),
    %       #{rows := [Resp]} = pgo:query(
    %           "SELECT password, passsalt FROM users WHERE login = $1 FOR UPDATE NOWAIT;",
    %           [Login],
    %           ?PGODECOPTS
    %       ),
    %       #{passsalt := PassSalt} = Resp,
    %       NewPassSalt = crypto:strong_rand_bytes(32),
    %       EncodedNewPassword =
    %       pgo:query(
    %           "UPDATE users SET password = $1, passsalt = $2 WHERE (login = $3 AND password = $4);",
    %           [EncodedNewPassword, NewPassSalt, Login, Password],
    %       )
    %   end),
    UserObj,
    ok.

%  -> [id,name]<-
get_users() ->
    case pgo:query(<<"SELECT id, name FROM users;">>, [], ?PGODECOPTS) of
        #{rows := Resp} ->
            Resp;
        _ ->
            [{error, <<"unavailable">>}]
    end.

 %%%%%%%%%%%%%%%%%
%---- Private ----%
 %%%%%%%%%%%%%%%%%
-spec handle_auth(bitstring(), 'ok' | tuple()) -> list().
handle_auth(Login, 'ok') ->
  Token = helpers:new_token(),
  TokenExpiration = helpers:new_token_expiration('new'),
  Resp =
    pgo:query(
      "UPDATE users SET token = $1, token_expiration = $2
       WHERE (login = $3) RETURNING token, token_expiration;",
      [Token, TokenExpiration, Login]
    ),
  case Resp of
    #{num_rows := 1, rows := Res} ->
      [{token_type, <<"Bearer">>}|Res];
    _ ->
      [{error, <<"oops! try again">>}]
  end;
handle_auth(_, {error, Error}) ->
  [{error, Error}];
handle_auth(Login, Password) ->
  handle_auth(Login, check_p(Password, check_l(Login))).

-spec check_p(bitstring(), tuple()) -> 'ok' | tuple().
check_p(Password, {'login_ok', {StoredPassword, StoredPassSalt}}) ->
  Res =
    helpers:encode_password(Password, StoredPassSalt) == StoredPassword,          % password match
  check_p('ok', {Res, <<"incorrect password">>});
check_p(_, {false, _Error}) ->
  %TODO log(_Error),
  {error, <<"incorrect login or password">>};
check_p(_, {true, _}) ->
  'ok'.

-spec check_l(bitstring()) -> tuple().
check_l(Login) ->
  Resp =
    pgo:query(
      "SELECT password, passsalt FROM users WHERE login = $1 FOR UPDATE NOWAIT;",
      [Login],
      ?PGODECOPTS
    ),
  case Resp of
    #{num_rows := 1, rows := [#{password := StoredPassword, passsalt := StoredPassSalt}]} ->
      {'login_ok', {StoredPassword, StoredPassSalt}};                               % login exists
    _ ->
      {false, <<"incorrect login">>}                                                % login missed
  end.