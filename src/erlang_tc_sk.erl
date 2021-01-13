-module(erlang_tc_sk).

-export([
    %% SecretKey API
    random/0,
    from_fr/1,
    public_key/1,
    reveal/1,
    sign/2
]).

-spec random() -> reference().
random() ->
    erlang_tc:sk_random().

-spec from_fr(Fr :: reference()) -> reference().
from_fr(Fr) ->
    erlang_tc:sk_from_fr(Fr).

-spec public_key(SK :: reference()) -> reference().
public_key(SK) ->
    erlang_tc:sk_public_key(SK).

-spec reveal(SK :: reference()) -> string().
reveal(SK) ->
    erlang_tc:sk_reveal(SK).

-spec sign(SK :: reference(), Msg :: binary()) -> reference().
sign(SK, Msg) ->
    erlang_tc:sk_sign(SK, Msg).
