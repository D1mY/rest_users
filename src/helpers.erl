-module(helpers).

-export([encode_password/2]).
-export([new_passsalt/0]).
-export([new_token_expiration/1]).
-export([new_token/0]).

-spec encode_password(binary(), binary()) -> binary().
encode_password(Password, PassSalt) ->
    crypto:pbkdf2_hmac(sha256, Password, PassSalt, 4096, 32).

-spec new_passsalt() -> binary().
new_passsalt() ->
    crypto:strong_rand_bytes(32).

-spec new_token_expiration('new' | integer()) -> integer().
new_token_expiration('new') ->
    new_token_expiration(os:system_time(seconds));
new_token_expiration(Time) ->
    Time + (60 * 60 * 24 * 30).

new_token() ->
    generate_fragment(32).

%%% from https://github.com/kivra/oauth2/blob/master/src/oauth2_token.erl
-spec generate_fragment(integer()) -> binary().
generate_fragment(0) ->
    <<>>;
generate_fragment(N) ->
    Rand = base64:encode(crypto:strong_rand_bytes(N)),
    Frag = <<<<C>> || <<C>> <= <<Rand:N/bytes>>, is_alphanum(C)>>,
    <<Frag/binary, (generate_fragment(N - byte_size(Frag)))/binary>>.

%% @doc Returns true for alphanumeric ASCII characters, false for all others.
-spec is_alphanum(char()) -> boolean().
is_alphanum(C) when C >= 16#30 andalso C =< 16#39 -> true;
is_alphanum(C) when C >= 16#41 andalso C =< 16#5A -> true;
is_alphanum(C) when C >= 16#61 andalso C =< 16#7A -> true;
is_alphanum(_) -> false.
