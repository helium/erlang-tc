-module(erlang_tc_pk_set).

-export([
    %% PublicKeySet API
    from_commitment/1,
    public_key/1,
    threshold/1,
    public_key_share/2,
    decrypt/3
]).

-type pk_set() :: reference().

-export_type([pk_set/0]).

-spec from_commitment(Commitment :: erlang_tc_commitment:commitment()) -> pk_set().
from_commitment(Commitment) ->
    erlang_tc:pk_set_from_commitment(Commitment).

-spec public_key(PKSet :: pk_set()) -> erlang_tc_pk:pk().
public_key(PKSet) ->
    erlang_tc:pk_set_public_key(PKSet).

-spec threshold(PKSet :: pk_set()) -> non_neg_integer().
threshold(PKSet) ->
    erlang_tc:pk_set_threshold(PKSet).

-spec public_key_share(PKSet :: pk_set(), I :: non_neg_integer()) -> erlang_tc_pk_share:pk_share().
public_key_share(PKSet, I) ->
    erlang_tc:pk_set_public_key_share(PKSet, I).

-spec decrypt(
    PKSet :: pk_set(),
    DecShares :: [{non_neg_integer(), erlang_tc_dec_share:dec_share()}],
    Cipher :: erlang_tc_ciphertext:ciphertext()
) -> erlang_tc_ciphertext:ciphertext().
decrypt(PKSet, DecShares, Cipher) ->
    erlang_tc:pk_set_decrypt(PKSet, DecShares, Cipher).
