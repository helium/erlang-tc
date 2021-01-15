-module(erlang_tc_sk_set).

-export([
    %% SecretKeySet API
    from_poly/1,
    public_keys/1,
    threshold/1,
    secret_key_share/2,
    random/1
]).

-type sk_set() :: reference().
-export_type([sk_set/0]).

-spec from_poly(Poly :: erlang_tc_poly:poly()) -> sk_set().
from_poly(Poly) ->
    erlang_tc:sk_set_from_poly(Poly).

-spec secret_key_share(SKSet :: sk_set(), I :: non_neg_integer()) -> erlang_tc_sk_share:sk_share().
secret_key_share(SKSet, I) ->
    erlang_tc:sk_set_secret_key_share(SKSet, I).

-spec threshold(SKSet :: sk_set()) -> non_neg_integer().
threshold(SKSet) ->
    erlang_tc:sk_set_threshold(SKSet).

-spec public_keys(SKSet :: sk_set()) -> erlang_tc_pk_set:pk_set().
public_keys(SKSet) ->
    erlang_tc:sk_set_public_keys(SKSet).

-spec random(Threshold :: non_neg_integer()) -> sk_set().
random(Threshold) ->
    erlang_tc:sk_set_random(Threshold).
