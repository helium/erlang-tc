-module(erlang_tc_sk_set).

-export([
    %% SecretKeySet API
    from_poly/1,
    public_keys/1,
    threshold/1,
    secret_key_share/2,
    random/1
]).

-spec from_poly(Poly :: reference()) -> reference().
from_poly(Poly) ->
    erlang_tc:sk_set_from_poly(Poly).

-spec secret_key_share(SKSet :: reference(), I :: non_neg_integer()) -> reference().
secret_key_share(SKSet, I) ->
    erlang_tc:sk_set_secret_key_share(SKSet, I).

-spec threshold(SKSet :: reference()) -> non_neg_integer().
threshold(SKSet) ->
    erlang_tc:sk_set_threshold(SKSet).

-spec public_keys(SKSet :: reference()) -> reference().
public_keys(SKSet) ->
    erlang_tc:sk_set_public_keys(SKSet).

-spec random(Threshold :: non_neg_integer()) -> reference().
random(Threshold) ->
    erlang_tc:sk_set_random(Threshold).
