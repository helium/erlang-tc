-module(erlang_tc_pk_set).

-export([
    %% PublicKeySet API
    from_commitment/1,
    public_key/1,
    threshold/1
]).

-spec from_commitment(Commitment :: reference()) -> reference().
from_commitment(Commitment) ->
    erlang_tc:pk_set_from_commitment(Commitment).

-spec public_key(PKSet :: reference()) -> reference().
public_key(PKSet) ->
    erlang_tc:pk_set_public_key(PKSet).

-spec threshold(PKSet :: reference()) -> non_neg_integer().
threshold(PKSet) ->
    erlang_tc:pk_set_threshold(PKSet).

